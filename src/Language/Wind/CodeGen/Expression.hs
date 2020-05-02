module Language.Wind.CodeGen.Expression (codegenExpr) where

import           Control.Monad.State
import qualified Data.Map                              as M
import           Data.String.Conversions
import           Debug.Trace                           (traceShow)
import qualified LLVM.AST                              as AST
import qualified LLVM.AST.Typed                        as AST
import qualified LLVM.IRBuilder.Constant               as L
import qualified LLVM.IRBuilder.Instruction            as L

import           Language.Wind.CodeGen.BinaryOperation
import           Language.Wind.CodeGen.Env
import           Language.Wind.CodeGen.Util
import           Language.Wind.SemanticAnalyser.AST

-- TODO: Split this into multiple functions/files
codegenExpr :: SAExpr -> Codegen AST.Operand
codegenExpr (TyInt, SALiteral i) = pure $ L.int32 (fromIntegral i)
codegenExpr (TyChar, SACharLiteral c) = pure $ L.int8 (fromIntegral c)
codegenExpr (TyDouble, SAFloatLiteral f) = pure $ L.double f
codegenExpr (TyBoolean, SABooleanLiteral b) = pure $ L.bit (if b then 1 else 0)
codegenExpr (_, SABinaryOp op lhs rhs) = do
  rhs' <- codegenExpr rhs
  lhs' <- codegenExpr lhs
  codegenBinaryOp op (fst lhs) (fst rhs) lhs' rhs'
codegenExpr (TyString, SAStringLiteral s) = do
  strs <- gets strings
  case M.lookup s strs of
    Nothing -> do
      let name = AST.mkName (show (M.size strs) <> ".str")
      op <- L.globalStringPtr (cs s) name
      modify $ \env -> env {strings = M.insert s (AST.ConstantOperand op) strs}
      pure (AST.ConstantOperand op)
    Just op -> pure op
codegenExpr (t, SAVarDeclaration (_, SAIdentifier n)) = do
  ltype <- typeToLLVMType t
  addr <- L.alloca ltype Nothing 0
  registerOperand n addr
  pure $ L.int32 0
codegenExpr (_, SAIdentifier name) = do
  addr <- gets ((M.! name) . operands)
  L.load addr 0
codegenExpr (_, SAVarInitialize (_, SAIdentifier n) expr) = do
  op <- codegenExpr expr
  addr <- L.alloca (AST.typeOf op) Nothing 0
  L.store addr 0 op
  registerOperand n addr
  pure op
codegenExpr (_, SACall name exps) = do
  exps' <- mapM (fmap (, []) . codegenExpr) exps
  f <- gets ((M.! name) . operands)
  L.call f exps'
codegenExpr t = traceShow t $ error "Internal error, unknown expression "
