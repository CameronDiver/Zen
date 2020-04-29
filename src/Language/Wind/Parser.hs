module Language.Wind.Parser where

import           Data.Text
import           Data.Void
import           Text.Megaparsec

-- TODO: Own instances of show
type Parser = Parsec Void Text

data Program =
  Program [Statement]
  deriving (Show)

data Statement
  = Expr Expr
  | VarDeclaration Expr
  deriving (Show)

data Expr
  = Literal Int
  | StringLiteral Text
  | CharLiteral Int
  | BinaryOp Op Expr Expr
  | Assign Expr Expr
  | Identifier Text
  | NoExpr
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  deriving (Show, Eq)
