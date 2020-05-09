module Language.Zen.CodeGen
  ( codegenProgram
  ) where

import           Control.Monad.State

import qualified LLVM.AST                          as AST
import qualified LLVM.AST.Type                     as AST
import qualified LLVM.IRBuilder.Module             as L

import qualified Data.Map                          as M
import           Data.String.Conversions

import           Language.Zen.SemanticAnalyser.AST

import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Function
import           Language.Zen.CodeGen.Util

codegenProgram :: SAProgram -> AST.Module
codegenProgram (SAProgram fns) =
  flip evalState (Env {operands = M.empty, strings = M.empty}) $
  L.buildModuleT "zen-prog" $ do
    emitBuiltins
    mapM codegenFunction fns

emitBuiltins :: LLVM ()
emitBuiltins = do
  forM_ builtinFunctions $ \(name, retT, paramTs) -> do
    func <- L.extern (AST.mkName $ cs name) paramTs retT
    registerOperand name func
  -- Also add the special case printf function
  printf <- L.externVarArgs (AST.mkName "printf") [stringPointer] AST.i32
  registerOperand "printf" printf
