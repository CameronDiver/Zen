module Language.Wind.CodeGen
  ( codegenProgram
  ) where

import           Control.Monad.State

import qualified LLVM.AST                              as AST
import qualified LLVM.AST.Type                         as AST
import qualified LLVM.AST.Typed                        as AST
import qualified LLVM.IRBuilder.Constant               as L
import qualified LLVM.IRBuilder.Instruction            as L
import qualified LLVM.IRBuilder.Module                 as L
import qualified LLVM.IRBuilder.Monad                  as L

import qualified Data.Map                              as M
import           Data.String.Conversions
import           Debug.Trace                           (traceShow)

import           Language.Wind.AST
import           Language.Wind.SemanticAnalyser.AST


import           Language.Wind.CodeGen.BinaryOperation
import           Language.Wind.CodeGen.Env
import           Language.Wind.CodeGen.Function
import           Language.Wind.CodeGen.Statement
import           Language.Wind.CodeGen.Util

codegenProgram :: SAProgram -> AST.Module
codegenProgram prg =
  flip evalState (Env {operands = M.empty, strings = M.empty}) $
  L.buildModuleT "wind-prog" $ do
    emitBuiltins
    codegenMain prg

emitBuiltins :: LLVM ()
emitBuiltins = do
  forM_ builtinFunctions $ \(name, retT, paramTs) -> do
    func <- L.extern (AST.mkName $ cs name) paramTs retT
    registerOperand name func
  -- Also add the special case printf function
  printf <- L.externVarArgs (AST.mkName "printf") [stringPointer] AST.i32
  registerOperand "printf" printf
