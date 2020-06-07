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

type Functions = M.Map Text FunctionInterface

data Env
  = Env
      { vars :: ScopeStack
      , functions :: Functions
      , currentFunction :: FunctionInterface
      , structs :: [SAStruct]
      }

type Semantic = ExceptT SemanticError (State Env)
