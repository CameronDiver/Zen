module Language.Wind.CodeGen.Util
  ( registerOperand
  , builtinFunctions
  , typeToLLVMType
  , stringPointer
  ) where

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

-- printf is handled as a special case at the moment
builtinFunctions :: [(Text, AST.Type, [AST.Type])]
builtinFunctions = [] --[("printf", AST.void, [AST.ptr AST.i8, AST.i32])]

stringPointer :: AST.Type
stringPointer = AST.ptr AST.i8

typeToLLVMType :: MonadState Env m => Type -> m AST.Type
typeToLLVMType t =
  case t of
    TyInt     -> pure AST.i32
    TyDouble  -> pure AST.double
    TyChar    -> pure AST.i8
    TyString  -> pure stringPointer
    TyBoolean -> pure AST.i1

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack
