module Language.Zen.CodeGen.Util
  ( registerOperand
  , builtinFunctions
  , typeToLLVMType
  , stringPointer
  ) where

import           Control.Monad.State
import           Data.Map                            as M
import           Data.String                         (fromString)
import           Data.String.Conversions
import           Data.Text                           (Text)
import qualified Data.Text                           as T

import qualified LLVM.AST                            as AST
import qualified LLVM.AST.Type                       as AST
import           LLVM.Prelude                        (ShortByteString)

import           Language.Zen.CodeGen.Env
import           Language.Zen.SemanticAnalyser.Types

registerOperand :: MonadState Env m => Text -> AST.Operand -> m ()
registerOperand name op =
  modify $ \env -> env {operands = M.insert name op (operands env)}

-- printf is handled as a special case at the moment
builtinFunctions :: [(Text, AST.Type, [AST.Type])]
builtinFunctions = [] --[("printf", AST.void, [AST.ptr AST.i8, AST.i32])]

stringPointer :: AST.Type
stringPointer = AST.ptr AST.i8

typeToLLVMType :: Type -> AST.Type
typeToLLVMType t =
  case t of
    TyInt -> AST.i32
    TyDouble -> AST.double
    TyChar -> AST.i8
    TyString -> stringPointer
    TyBoolean -> AST.i1
    TyVoid -> AST.VoidType
    TyArray t' -> AST.ptr (typeToLLVMType t')
    _ -> error $ "typeToLLVMType not implemented for type " <> show t

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack
