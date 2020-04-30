module Language.Wind.SemanticAnalyser.AST where

import           Data.Text

import           Language.Wind.AST

-- The flexible type means it can be coverted to any other
-- type, useful for variable declarations which have not
-- been provided with a type and also have not been
-- initialized
data Type
  = TyInt
  | TyFloat
  | TyString
  | TyChar
  | TyObject
  | TyFunction
  | TyFlexible
  | TyVoid
  deriving (Show, Eq)

type SAExpr = (Type, SAExpr')

data SAExpr'
  = SALiteral Int
  | SAStringLiteral Text
  | SACharLiteral Int
  | SABinaryOp Op SAExpr SAExpr
  | SAAssign SAExpr SAExpr
  | SAIdentifier Text
  | SAVarDeclaration SAExpr
  | SAVarInitialize SAExpr SAExpr
  | SANoExpr
  deriving (Show, Eq)

data SAStatement =
  SAExpr SAExpr
  deriving (Show)

data SAProgram =
  SAProgram [SAStatement]
  deriving (Show)

data VarScope
  = Global
  | Local
  deriving (Show, Eq, Ord)

isNumeric :: Type -> Bool
isNumeric t =
  case t of
    TyInt   -> True
    TyFloat -> True
    TyChar  -> True
    _       -> False
