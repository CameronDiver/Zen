module Language.Zen.CodeGen.BinaryOperation
  ( codegenBinaryOp
  ) where

import           Debug.Trace                       (traceShow)
import qualified LLVM.AST                          as AST
import qualified LLVM.IRBuilder.Instruction        as L

import           Language.Zen.AST
import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Util
import           Language.Zen.SemanticAnalyser.AST

codegenBinaryOp ::
     Operator
  -> Type
  -> Type
  -> AST.Operand
  -> AST.Operand
  -> Codegen AST.Operand
codegenBinaryOp op lht rht lhs rhs =
  case op of
    Add ->
      case (lht, rht) of
        (TyInt, TyInt) -> L.add lhs rhs
        (TyDouble, TyDouble) -> L.fadd lhs rhs
        (TyInt, TyDouble) -> toFloatingPoint lht lhs >>= flip L.fadd rhs
        (TyDouble, TyInt) -> toFloatingPoint rht rhs >>= L.fadd lhs
        ty -> traceShow ty $ error "Not sure how to add values"
    Sub ->
      case (lht, rht) of
        (TyInt, TyInt) -> L.sub lhs rhs
        (TyDouble, TyDouble) -> L.fsub lhs rhs
        (TyDouble, TyInt) -> toFloatingPoint rht rhs >>= L.fsub lhs
        (TyInt, TyDouble) -> toFloatingPoint lht lhs >>= flip L.fsub rhs
        _ -> traceShow (lht, rht) $ error "Cant subtract types"
    Mul ->
      case (lht, rht) of
        (TyInt, TyInt) -> L.mul lhs rhs
        (TyDouble, TyDouble) -> L.fmul lhs rhs
        (TyInt, TyDouble) -> toFloatingPoint lht lhs >>= flip L.fmul rhs
        (TyDouble, TyInt) -> toFloatingPoint rht rhs >>= flip L.fmul lhs
        _ -> traceShow (lht, rht) $ error "Cant multiply types"
    Div ->
      case (lht, rht) of
        (TyInt, TyInt) -> L.sdiv lhs rhs
        (TyInt, TyDouble) -> toFloatingPoint lht lhs >>= flip L.fdiv rhs
        (TyDouble, TyInt) -> toFloatingPoint rht rhs >>= L.fdiv lhs
        (TyDouble, TyDouble) -> L.fdiv lhs rhs
        _              -> traceShow (lht, rht) $ error "Cant divide types"
    -- _ -> traceShow (op, lht, rht) $ error "Unimplemented binary op codegen"

toFloatingPoint :: Type -> AST.Operand -> Codegen AST.Operand
toFloatingPoint TyInt value = do
  lltype <- typeToLLVMType TyDouble
  L.sitofp value lltype
toFloatingPoint t _ = error $ "Can't convert " <> show t <> " to floating point"

