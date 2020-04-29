module Main where

import           Language.Wind

main :: IO ()
main = do
  _ <- compile $ CompileOpts "test" "test" "let a=123;a=3+3;4+3;"
  print "done"
