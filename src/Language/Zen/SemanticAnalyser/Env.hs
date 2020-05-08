module Language.Zen.SemanticAnalyser.Env
  ( Env(..)
  , Semantic
  , Functions
  ) where

import           Data.Map                            as M
import           Data.Text                           (Text)

import           Control.Monad.Except
import           Control.Monad.State

import           Language.Zen.SemanticAnalyser.AST
import           Language.Zen.SemanticAnalyser.Error
import           Language.Zen.SemanticAnalyser.Scope

-- TODO: Also store whether these values are const
type Functions = M.Map Text Function

-- Currently we don't really need to store anything
-- for the scope, only that they exist but we may need
-- to change this data type in the future
data Env
  = Env
      { vars :: ScopeStack
      , functions :: Functions
      }

type Semantic = ExceptT SemanticError (State Env)
