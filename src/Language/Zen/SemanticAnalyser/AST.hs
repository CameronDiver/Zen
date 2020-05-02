module Language.Zen.SemanticAnalyser.AST where

import           Data.Text
import           Data.Text.Prettyprint.Doc

import           Language.Zen.AST

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

type SAExpr = (Type, SAExpr')

data SAExpr'
  = SALiteral Int
  | SAFloatLiteral Double
  | SAStringLiteral Text
  | SACharLiteral Int
  | SABooleanLiteral Bool
  | SABinaryOp Operator SAExpr SAExpr
  | SAAssign SAExpr SAExpr
  | SAIdentifier Text
  | SAVarDeclaration SAExpr
  | SAVarInitialize SAExpr SAExpr
  | SACall Text [SAExpr]
  | SANoExpr
  deriving (Show, Eq)

data SAStatement
  = SAExpr SAExpr
  | SAIfStatement
      { predicate :: SAExpr
      , ifBody :: [SAStatement]
      , elsebody :: [SAStatement]
      }
  deriving (Show)

data SAProgram =
  SAProgram [SAStatement]
  deriving (Show)

data VarScope
  = Global
  | Local
  deriving (Show, Eq, Ord)

data Function
  = Function
      { returnType :: Type
      , name :: Text
      , params :: [SAExpr]
      }

isNumeric :: Type -> Bool
isNumeric t =
  case t of
    TyInt    -> True
    TyDouble -> True
    TyChar   -> True
    _        -> False

instance Pretty Type where
  pretty t = case t of
    TyInt      -> "Integer"
    TyChar     -> "Char"
    TyDouble   -> "Double"
    TyFlexible -> "Flexible"
    TyString   -> "String"
    TyObject   -> "Object"
    TyVoid     -> "Void"
    TyFunction -> "Function"
    TyBoolean  -> "Boolean"
