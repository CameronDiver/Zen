module Language.Zen.SemanticAnalyser.Error where

import           Data.Text                         (Text)

import           Data.Text.Prettyprint.Doc
import           Language.Zen.AST
import           Language.Zen.SemanticAnalyser.AST

type Name = Text

data SemanticError
  = TypeError
      { loc :: Location
      , expected :: [Type]
      , got :: Type
      }
  | UndefinedSymbol
      { loc :: Location
      , name :: Text
      }
  | InvalidAssignmentLval
      { loc :: Location
      , lval :: Expr
      }
  | InvalidVarDeclaration
      { loc :: Location
      , target :: Expr
      }
  | DuplicateVarDeclaration
      { loc :: Location
      , name :: Text
      }
  | InvalidArgumentCount
      { loc :: Location
      , required :: Int
      , provided :: Int
      }
  deriving (Show)


instance Pretty SemanticError where
  pretty e = case e of
    UndefinedSymbol loc name ->
      "Use of undefined symbol" <+> pretty name <> showLoc loc
    TypeError loc ts t ->
      "Type mismatch, expected"
        <+> (commasep $ map pretty ts)
        <+> "but got"
        <+> pretty t
        <>  showLoc loc
    InvalidAssignmentLval loc lval ->
      "Cannot assign to expression:" <+> pretty lval <> showLoc loc
    InvalidVarDeclaration loc target ->
      "Cannot declare expression as variable:" <+> pretty target <> showLoc loc
    -- TODO: It would be nice to show the previous
    -- declaration location
    DuplicateVarDeclaration loc name ->
      "Cannot redeclare variable:" <+> pretty name <> showLoc loc
    InvalidArgumentCount loc req prov ->
      "Mismatched argument count; required"
        <+> pretty req
        <>  ","
        <+> "but got"
        <+> pretty prov
        <>  showLoc loc

showLoc :: Location -> Doc ann
showLoc (Location fileno filename _) =
  hardline <+> "at" <+> pretty filename <> ":" <> pretty fileno

commasep :: [Doc ann] -> Doc ann
commasep = concatWith (\x y -> x <> "," <+> y)
