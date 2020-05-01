module Language.Wind.CodeGen
  ( codegenProgram
  ) where

import           Control.Monad.State

import qualified LLVM.AST                           as AST
import qualified LLVM.AST.Constant                  as C
import qualified LLVM.AST.Type                      as AST
import qualified LLVM.AST.Typed                     as AST
import qualified LLVM.IRBuilder.Constant            as L
import qualified LLVM.IRBuilder.Instruction         as L
import qualified LLVM.IRBuilder.Module              as L
import qualified LLVM.IRBuilder.Monad               as L

import qualified Data.Map                           as M
import           Data.String.Conversions
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Debug.Trace                        (traceShow)

import           Language.Wind.AST
import           Language.Wind.CodeGen.Env
import           Language.Wind.CodeGen.Function
import           Language.Wind.CodeGen.Util
import           Language.Wind.SemanticAnalyser.AST

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

codegenProgram :: SAProgram -> AST.Module
codegenProgram prg =
  flip evalState (Env {operands = M.empty, strings = M.empty}) $
    L.buildModuleT "wind-prog" $ do
      emitBuiltins
      codegenMain prg codegenStatement

emitBuiltins :: LLVM ()
emitBuiltins =
  forM_ builtinFunctions $ \(name, retT, paramTs) -> do
    func <- L.extern (AST.mkName $ cs name) paramTs retT
    registerOperand name func

codegenStatement :: SAStatement -> Codegen ()
codegenStatement stmt =
  case stmt of
    SAExpr e -> void $ codegenExpr e

codegenExpr :: SAExpr -> Codegen AST.Operand
codegenExpr (TyInt, SALiteral i) = pure $ L.int32 (fromIntegral i)
codegenExpr (TyChar, SACharLiteral c) = pure $ L.int8 (fromIntegral c)
codegenExpr (t, SABinaryOp op lhs rhs) = do
  rhs' <- codegenExpr rhs
  lhs' <- codegenExpr lhs
  case op of
    Add ->
      case (fst lhs, fst rhs) of
        (TyInt, TyInt) -> L.add lhs' rhs'
        (TyFloat, TyFloat) -> L.fadd lhs' rhs'
        ty -> traceShow ty $ error "Not sure how to add values"
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
