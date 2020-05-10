module Language.Zen.SemanticAnalyser.Types where

import           Data.Text                 (Text)
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

typeFromText :: Text -> Maybe Type
typeFromText t =
  case t of
    "int"    -> Just TyInt
    "char"   -> Just TyChar
    "double" -> Just TyDouble
    "string" -> Just TyString
    "void"   -> Just TyVoid
    "bool"   -> Just TyBoolean
    _        -> Nothing
