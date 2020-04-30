module Language.Wind.CodeGen.Util where

import           Control.Monad.State
import           Data.Map                           as M
import           Data.String                        (fromString)
import           Data.String.Conversions
import           Data.Text                          (Text)
import qualified Data.Text                          as T

import qualified LLVM.AST                           as AST
import qualified LLVM.AST.Type                      as AST
import           LLVM.Prelude                       (ShortByteString)

import           Language.Wind.CodeGen.Env
import           Language.Wind.SemanticAnalyser.AST

registerOperand :: MonadState Env m => Text -> AST.Operand -> m ()
registerOperand name op =
  modify $ \env -> env {operands = M.insert name op (operands env)}

typeToLLVMType :: MonadState Env m => Type -> m AST.Type
typeToLLVMType t =
  case t of
    TyInt    -> pure AST.i32
    TyFloat  -> pure AST.double
    TyChar   -> pure AST.i8
    TyString -> pure $ AST.ptr AST.i8

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack
