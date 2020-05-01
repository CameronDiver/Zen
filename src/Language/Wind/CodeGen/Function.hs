module Language.Wind.CodeGen.Function
  ( codegenMain
  ) where

import           Control.Monad.Fix
import           Control.Monad.State

import qualified LLVM.AST                           as AST
import qualified LLVM.AST.Type                      as AST
import qualified LLVM.IRBuilder.Constant            as L
import qualified LLVM.IRBuilder.Instruction         as L
import qualified LLVM.IRBuilder.Module              as L
import qualified LLVM.IRBuilder.Monad               as L

import           Language.Wind.CodeGen.Env
import           Language.Wind.CodeGen.Util
import           Language.Wind.SemanticAnalyser.AST

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

codegenMain :: SAProgram -> (SAStatement -> Codegen ()) -> LLVM ()
codegenMain (SAProgram stmts) fn =
  void $
  -- Recursively define and use `fun`, we do this in case a
  -- function calls itself (this isn't a worry with main,
  -- but it will be when we have user defined functions)
  -- We don't use mdo here because it messes up formatters
  -- and ides
  mfix
    (\fun -> do
       registerOperand "main" fun
       let returnType = AST.i8
       fun <- L.function name [] returnType genBody
       pure fun)
  where
    name = AST.mkName "main"
    genBody :: [AST.Operand] -> Codegen ()
    genBody _ = do
      _entry <- L.block `L.named` "entry"
      mapM_ fn stmts
      L.ret $ L.int8 0
