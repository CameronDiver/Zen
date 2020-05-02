module Language.Wind.AST where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

data Program =
  Program [Statement]
  deriving (Show)

data Statement
  = Expr Expr
  | If
      { stmtLoc :: Location
      , predicate :: Expr
      , ifBody :: [Statement]
      , elseBody :: [Statement]
      }
  deriving (Show)

-- We add a Location to every AST node directly into the
-- datatype, but this would be better using something like
-- the CoFree monad, where we can annotate it seperately
data Expr
  = Literal
      { exprLoc :: Location
      , intVal :: Int
      }
  | StringLiteral
      { exprLoc :: Location
      , strVal :: Text
      }
  | BooleanLiteral
      { exprLoc :: Location
      , boolVal :: Bool
      }
  | CharLiteral
      { exprLoc :: Location
      , charVal :: Int
      }
  | FloatLiteral
      { exprLoc :: Location
      , doubleVal :: Double
      }
  | BinaryOp
      { exprLoc :: Location
      , op :: Operator
      , lhs :: Expr
      , rhs :: Expr
      }
  | Assign
      { exprLoc :: Location
      , lhs :: Expr
      , rhs :: Expr
      }
  | Identifier
      { exprLoc :: Location
      , idText :: Text
      }
  | VarDeclaration
      { exprLoc :: Location
      , decl :: Expr
      }
  | Call
      { exprLoc :: Location
      , nameText :: Text
      , argExprs :: [Expr]
      }
  | NoExpr
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  deriving (Show, Eq)

data Location
  = Location
      { locLineno :: Int
      , locFilename :: Text
      , locColumn :: Int
      }
  deriving (Show, Eq)

instance Pretty Operator where
  pretty op = case op of
    Add -> "+"
    Sub -> "-"

instance Pretty Expr where
  pretty e = case e of
    Literal      _ i -> pretty i
    FloatLiteral _ f -> pretty f
    BooleanLiteral _ b -> pretty b
    Call _ c exps ->
      pretty c <+> concatWith (\x y -> x <> ", " <> y) (map pretty exps)
    StringLiteral _ s     -> dquotes $ pretty s
    CharLiteral   _ c     -> squotes $ pretty c
    BinaryOp _ op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
    Assign _ lhs rhs      -> pretty lhs <+> "=" <+> pretty rhs
    Identifier _ i        -> pretty i
    NoExpr                -> mempty
    VarDeclaration _ n    -> "let " <> pretty n <> semi

instance Pretty Statement where
  pretty s = case s of
    Expr e -> pretty e <> semi

instance Pretty Program where
  pretty (Program statements) = hardsep $ map pretty statements

-- Separates many docs with hardlines
hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)
