module Language.Zen.CodeGen.Function
  ( codegenMain
  , codegenFunction
  ) where

import           Control.Monad                     (forM_)
import           Data.String.Conversions

import qualified LLVM.AST                          as AST
import qualified LLVM.AST.Type                     as AST
import qualified LLVM.AST.Typed                    as AST
import qualified LLVM.IRBuilder.Constant           as L
import qualified LLVM.IRBuilder.Instruction        as L
import qualified LLVM.IRBuilder.Module             as L
import qualified LLVM.IRBuilder.Monad              as L

import           Language.Zen.CodeGen.Env
import           Language.Zen.CodeGen.Statement
import           Language.Zen.CodeGen.Util
import           Language.Zen.SemanticAnalyser.AST

codegenMain :: [SAStatement] -> LLVM ()
codegenMain stmts
  -- Recursively define and use `fun`, we do this in case a
  -- function calls itself
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

codegenFunction :: SAFunction -> LLVM ()
codegenFunction (SAFunction name args returnT body) =
  mdo registerOperand name fn
      let retTy = typeToLLVMType returnT
      fn <- L.function (AST.mkName $ cs name) (fmap makeArg args) retTy genBody
      pure ()
  where
    makeArg (t, n) = (typeToLLVMType t, L.ParameterName $ cs n)
    genBody :: [AST.Operand] -> Codegen ()
    genBody ops = do
      _entry <- L.block `L.named` "entry"
      forM_
        (zip ops args)
        (\(op, (_, n)) -> do
           addr <- L.alloca (AST.typeOf op) Nothing 0
           L.store addr 0 op
           registerOperand n addr)
      mapM_ codegenStatement body
