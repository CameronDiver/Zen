module Language.Wind.AST where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

data Program =
  Program [Statement]
  deriving (Show)

data Statement =
  Expr Expr
  deriving (Show)

data Expr
  = Literal Int
  | StringLiteral Text
  | CharLiteral Int
  | BinaryOp Op Expr Expr
  | Assign Expr Expr
  | Identifier Text
  | VarDeclaration Expr
  | NoExpr
  deriving (Show, Eq)

data Op
  = Add
  | Sub
  deriving (Show, Eq)

instance Pretty Op where
  pretty op =
    case op of
      Add -> "+"
      Sub -> "-"

instance Pretty Expr where
  pretty e =
    case e of
      Literal i           -> pretty i
      StringLiteral s     -> dquotes $ pretty s
      CharLiteral c       -> squotes $ pretty c
      BinaryOp op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
      Assign lhs rhs      -> pretty lhs <+> "=" <+> pretty rhs
      Identifier i        -> pretty i
      NoExpr              -> mempty
      VarDeclaration n    -> "let " <> pretty n <> semi

instance Pretty Statement where
  pretty s =
    case s of
      Expr e -> pretty e <> semi

instance Pretty Program where
  pretty (Program statements) = hardsep $ map pretty statements

-- Separates many docs with hardlines
hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)
