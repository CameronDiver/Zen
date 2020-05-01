module Language.Wind.SemanticAnalyser.Error where

import           Data.Text

import           Language.Wind.AST
import           Language.Wind.SemanticAnalyser.AST

type Name = Text

data SemanticError
  = TypeError
      { expected :: [Type]
      , got :: Type
      }
  | UndefinedSymbol
      { name :: Text
      }
  | InvalidAssignmentLval
      { lval :: Expr
      }
  | InvalidVarDeclaration
      { target :: Expr
      }
  | DuplicateVarDeclaration
      { name :: Text
      }
  | InvalidArgumentCount
      { required :: Int
      , provided :: Int
      }
  deriving (Show)
