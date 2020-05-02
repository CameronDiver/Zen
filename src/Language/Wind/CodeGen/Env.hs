module Language.Wind.CodeGen.Env
  ( Env(..), LLVM, Codegen
  ) where

import           Data.Map  as M
import           Data.Text (Text)
import qualified LLVM.AST  as AST
import qualified LLVM.IRBuilder.Monad               as L
import qualified LLVM.IRBuilder.Module               as L
import Control.Monad.State

data Env
  = Env
      { operands :: M.Map Text AST.Operand
      , strings :: M.Map Text AST.Operand
      }
  deriving (Eq, Show)

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM
