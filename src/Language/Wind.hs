module Language.Wind where

import           Data.Text
import           Text.Megaparsec          (runParser)
import           Text.Megaparsec.Error    (errorBundlePretty)

import           Language.Wind.Combinator
import           Language.Wind.Lexemes
import           Language.Wind.Parser


data CompileOpts = CompileOpts { compileOptsFilename :: String
                               , compileOptsOutput :: String
                               , compileOptsInput :: Text
                               }

compile :: CompileOpts -> IO ()
compile (CompileOpts filename output input) = do
  putStrLn $ "Compiling " ++ filename
  putStrLn $ " into " ++ output
  let parseTree = runParser programParser filename input
  case parseTree of
    Left err  -> putStrLn $ errorBundlePretty  err
    Right ast -> print ast
  return ()
