module Language.Zen.CodeGen.Statement (codegenStatement) where

import           Control.Monad
import           Debug.Trace                       (traceShow)
import qualified LLVM.AST                          as AST
import qualified LLVM.IRBuilder.Instruction        as L
import qualified LLVM.IRBuilder.Monad              as L

import           Language.Zen.AST
import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Expression
import           Language.Zen.CodeGen.Util
import           Language.Zen.SemanticAnalyser.AST

codegenStatement :: SAStatement -> Codegen ()
codegenStatement stmt =
  case stmt of
    SAExpr e                -> void $ codegenExpr e
    s@(SAIfStatement _ _ _) -> codegenIfStatement s

-- Ideally we'd use mfix here, rather than mdo (which messes
-- up my editor and formatters to no end) but that gives an
-- inifinite loop which I was unable to debug successfully
codegenIfStatement :: SAStatement -> Codegen ()
codegenIfStatement (SAIfStatement predicate iBody eBody) = mdo
    bool <- codegenExpr predicate
    L.condBr bool thenBlock elseBlock

    thenBlock <- L.block `L.named` "then"
    mapM_ codegenStatement iBody
    mkTerminator $ L.br mergeBlock

    elseBlock <- L.block `L.named` "else"
    mapM_ codegenStatement eBody
    mkTerminator $ L.br mergeBlock

    mergeBlock <- L.block `L.named` "merge"
    pure ()
codegenIfStatement _ = error "If statement not passed to generator if statement"


mkTerminator :: Codegen () -> Codegen ()
mkTerminator instr = do
  check <- L.hasTerminator
  unless check instr
