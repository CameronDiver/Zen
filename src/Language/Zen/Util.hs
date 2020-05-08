module Language.Zen.Util where

import           Control.Monad.State

locally :: MonadState s m => m a -> m a
locally computation
  -- Get the current set of variables
 = do
  oldState <- get
  result <- computation
  put oldState
  return result
