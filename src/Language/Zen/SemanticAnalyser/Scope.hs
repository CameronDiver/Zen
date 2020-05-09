module Language.Zen.SemanticAnalyser.Scope where

import           Data.List                           (find)
import           Data.List.Extra                     (firstJust)
import           Data.Text                           (Text)

import           Language.Zen.SemanticAnalyser.Types

data Scope
  = Scope
      { name :: Text
      , variables :: [(Type, Text)]
      }
  deriving (Show, Eq)

type ScopeStack = [Scope]

newScopeStack :: ScopeStack
newScopeStack = [Scope "Global" []]

pushScope :: ScopeStack -> Scope -> ScopeStack
pushScope scopes sc = sc : scopes

-- TODO: Make sure we don't pop an empty list
popScope :: ScopeStack -> ScopeStack
popScope = tail

addVariable :: ScopeStack -> (Type, Text) -> ScopeStack
addVariable scopes var =
  topScope {variables = var : variables topScope} : tail scopes
  where
    topScope = head scopes

findVariable :: ScopeStack -> Text -> Maybe (Type, Text)
findVariable ss name = firstJust find' ss
  where
    find' (Scope _ vars) = find (\(_, t) -> t == name) vars
