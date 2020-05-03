module Language.Zen.CodeGen.Function
  ( codegenMain
  ) where

import qualified LLVM.AST                          as AST
import qualified LLVM.AST.Type                     as AST
import qualified LLVM.IRBuilder.Constant           as L
import qualified LLVM.IRBuilder.Instruction        as L
import qualified LLVM.IRBuilder.Module             as L
import qualified LLVM.IRBuilder.Monad              as L

import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Statement
import           Language.Zen.CodeGen.Util
import           Language.Zen.SemanticAnalyser.AST

codegenMain :: SAProgram -> LLVM ()
codegenMain (SAProgram stmts)
  -- Recursively define and use `fun`, we do this in case a
  -- function calls itself (this isn't a worry with main,
  -- but it will be when we have user defined functions)
 =
  mdo registerOperand "main" fun
      let returnType = AST.i8
      fun <- L.function name [] returnType genBody
      pure ()
  where
    name = AST.mkName "main"
    genBody :: [AST.Operand] -> Codegen ()
    genBody _ = do
      _entry <- L.block `L.named` "entry"
      mapM_ codegenStatement stmts
      L.ret $ L.int8 0
