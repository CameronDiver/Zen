module Language.Zen.SemanticAnalyser.AST where

import           Data.Text

import           Language.Zen.AST
import           Language.Zen.SemanticAnalyser.Types

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
  | SAReturn (Maybe SAExpr)
  | SACall Text [SAExpr]
  | SAIndex SAExpr SAExpr
  | SANoExpr
  deriving (Show, Eq)

data SAStatement
  = SAExpr SAExpr
  | SAIfStatement
      { predicate :: SAExpr
      , ifBody :: [SAStatement]
      , elsebody :: [SAStatement]
      }
  | SAWhileStatement
      { predicate :: SAExpr
      , body :: [SAStatement]
      }
  deriving (Show)

data SAFunction
  = SAFunction
      { name :: Text
      , args :: [(Type, Text)]
      , returnT :: Type
      , body :: [SAStatement]
      }
  deriving (Show)

newtype SAProgram =
  SAProgram [SAFunction]
  deriving (Show)

data VarScope
  = Global
  | Local
  deriving (Show, Eq, Ord)

data FunctionInterface
  = FunctionInterface
      { name :: Text
      , returnType :: Type
      , params :: [(Type, Text)]
      }

isNumeric :: Type -> Bool
isNumeric t =
  case t of
    TyInt    -> True
    TyDouble -> True
    TyChar   -> True
    _        -> False
