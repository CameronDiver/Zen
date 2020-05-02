module Language.Zen.CodeGen.BinaryOperation (codegenBinaryOp) where

import           Control.Monad
import           Debug.Trace                       (traceShow)
import qualified LLVM.AST                          as AST
import qualified LLVM.IRBuilder.Instruction        as L

import           Language.Zen.AST
import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Util
import           Language.Zen.SemanticAnalyser.AST

codegenBinaryOp :: Operator -> Type -> Type -> AST.Operand -> AST.Operand-> Codegen AST.Operand
codegenBinaryOp op lht rht lhs rhs =
  case op of
    Add ->
      case (lht, rht) of
        (TyInt, TyInt) -> L.add lhs rhs
        (TyDouble, TyDouble) -> L.fadd lhs rhs
        (TyDouble, t) -> do
          unless (elem t [TyChar, TyInt]) $ error ("Cant add double and " <> show t)
          lltype <- typeToLLVMType TyDouble
          cast <- L.sitofp rhs lltype
          L.fadd lhs cast
        (TyInt, t) ->
          case t of
            TyChar -> L.add lhs rhs
            TyDouble -> do
              lltype <- typeToLLVMType TyDouble
              cast <- L.sitofp lhs lltype
              L.fadd cast rhs
        ty -> traceShow ty $ error "Not sure how to add values"
    Sub ->
      case (lht, rht) of
        (TyInt, TyInt)      -> L.sub lhs rhs
        (TyDouble,TyDouble) -> L.fsub lhs rhs
        (TyDouble, t) -> do
          unless (elem t [TyChar, TyInt]) $ error ("Cant subtract double and " <> show t)
          lltype <- typeToLLVMType TyDouble
          cast <- L.sitofp rhs lltype
          L.fsub lhs cast
        (TyInt, t) -> case t of
            TyChar -> L.sub lhs rhs
            TyDouble -> do
              lltype <- typeToLLVMType TyDouble
              cast <- L.sitofp lhs lltype
              L.fsub cast rhs
