module Language.Zen.SemanticAnalyser.Types where

import           Data.Maybe                (fromJust, isNothing)
import           Data.Text                 as T
import           Data.Text.Prettyprint.Doc

-- The flexible type means it can be coverted to any other
-- type, useful for variable declarations which have not
-- been provided with a type and also have not been
-- initialized
data Type
  = TyInt
  | TyDouble
  | TyString
  | TyChar
  | TyBoolean
  | TyObject
  | TyFunction
  | TyFlexible
  | TyVoid
  | TyArray Type
  deriving (Show, Eq)

instance Pretty Type where
  pretty t =
    case t of
      TyInt      -> "Integer"
      TyChar     -> "Char"
      TyDouble   -> "Double"
      TyFlexible -> "Flexible"
      TyString   -> "String"
      TyObject   -> "Object"
      TyVoid     -> "Void"
      TyFunction -> "Function"
      TyBoolean  -> "Boolean"
      TyArray t' -> "Array of " <> pretty t'

typeFromText :: Text -> Maybe Type
typeFromText t =
  case t of
    "int" -> Just TyInt
    "char" -> Just TyChar
    "double" -> Just TyDouble
    "string" -> Just TyString
    "void" -> Just TyVoid
    "bool" -> Just TyBoolean
    -- FIXME: Clean up this terrible code
    _ ->
      if not $ T.null t
        then do
          let start = T.head t
          let end = T.head $ T.reverse t
          if start == '[' && end == ']'
            then do
              let len = T.length t
              let inner = typeFromText $ T.take (len - 2) $ T.drop 1 t
              if isNothing inner
                then Nothing
                else Just $ TyArray $ fromJust inner
            else Nothing
        else Nothing

unwrapType :: Type -> Type
unwrapType (TyArray t) = t
unwrapType t           = error $ "Cannot unwrap a non-wrapped type" <> show t

isWrappedType :: Type -> Bool
isWrappedType (TyArray _) = True
isWrappedType _           = False
