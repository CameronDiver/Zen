module Language.Zen.AST where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

newtype Program =
  Program [TopLevel]
  deriving (Show)

data TopLevel
  = Statement Statement
  | Fn FunctionDef
  deriving (Show)

data FunctionArg
  = FunctionArg
      { location :: Location
      , name :: Text
      , argType :: Text
      }
  deriving (Show)

data FunctionDef
  = FunctionDef
      { stmtLoc :: Location
      , name :: Text
      , args :: [FunctionArg]
      , returnT :: Text
      , returnTLoc :: Location
      , body :: [Statement]
      }
  deriving (Show)

data Statement
  = Expr Expr
  | If
      { stmtLoc :: Location
      , predicate :: Expr
      , ifBody :: [Statement]
      , elseBody :: [Statement]
      }
  | While
      { stmtLoc :: Location
      , predicate :: Expr
      , body :: [Statement]
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
  | Return
      { exprLoc :: Location
      , returnExpr :: Maybe Expr
      }
  | Index
      { exprLoc :: Location
      , source :: Expr
      , index :: Expr
      }
  | NoExpr
  deriving (Show, Eq)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | NEq
  | Less
  | Greater
  | LessEq
  | GreaterEq
  deriving (Show, Eq)

data Location
  = Location
      { locLineno :: Int
      , locFilename :: Text
      , locColumn :: Int
      }
  deriving (Show, Eq)

instance Pretty Operator where
  pretty op =
    case op of
      Add       -> "+"
      Sub       -> "-"
      Mul       -> "*"
      Div       -> "/"
      Eq        -> "=="
      NEq       -> "!="
      Less      -> "<"
      Greater   -> ">"
      LessEq    -> "<="
      GreaterEq -> ">="

instance Pretty Expr where
  pretty e =
    case e of
      Literal _ i -> pretty i
      FloatLiteral _ f -> pretty f
      BooleanLiteral _ b -> pretty b
      Call _ c exps ->
        pretty c <+> concatWith (\x y -> x <> ", " <> y) (fmap pretty exps)
      StringLiteral _ s -> dquotes $ pretty s
      CharLiteral _ c -> squotes $ pretty c
      BinaryOp _ op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
      Assign _ lhs rhs -> pretty lhs <+> "=" <+> pretty rhs
      Identifier _ i -> pretty i
      NoExpr -> mempty
      VarDeclaration _ n -> "let " <> pretty n <> semi
      Index _ n idx -> pretty n <> "[" <> pretty idx <> "]"
      Return _ v -> "return " <> pretty v <> semi

instance Pretty Statement where
  pretty s =
    case s of
      Expr e -> pretty e <> semi
      _      -> pretty $ show s

instance Pretty TopLevel where
  pretty s = pretty $ show s

instance Pretty Program where
  pretty (Program statements) = hardsep $ fmap pretty statements

-- Separates many docs with hardlines
hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)
