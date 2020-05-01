module Language.Wind.CodeGen.Env
  ( Env(..)
  ) where

import           Data.Map  as M
import           Data.Text (Text)
import qualified LLVM.AST  as AST

data Env
  = Env
      { operands :: M.Map Text AST.Operand
      , strings :: M.Map Text AST.Operand
      }
  deriving (Eq, Show)
