module Language.Wind where

import           Data.String.Conversions
import           Data.Text
import           Data.Text.IO                       as T
import           Data.Void
import qualified LLVM.AST                           as AST
import           LLVM.Pretty
import           Text.Megaparsec                    (runParser)
import           Text.Megaparsec.Error              (ParseErrorBundle,
                                                     errorBundlePretty)

import           System.IO
import           System.IO.Temp
import           System.Process

import           Language.Wind.AST
import           Language.Wind.CodeGen
import           Language.Wind.Combinator
import           Language.Wind.SemanticAnalyser
import           Language.Wind.SemanticAnalyser.AST

data CompileOpts
  = CompileOpts
      { compileOptsFilename :: String
      , compileOptsOutput :: String
      , compileOptsInput :: Text
      }

generateAST :: String -> Text -> Either (ParseErrorBundle Text Void) Program
generateAST = runParser programParser

analyseAST :: Program -> Either SemanticError SAProgram
analyseAST = checkProgram

generateLLVM :: SAProgram -> AST.Module
generateLLVM = codegenProgram

-- TODO: Implement this with a JIT
executeModule :: AST.Module -> IO String
executeModule m = withSystemTempFile "wind-exe" execute'
  where
    execute' fp handle = do
      hClose handle
      compileModule m fp
      readProcess fp [] []

compileModule :: AST.Module -> FilePath -> IO ()
compileModule m exePath = withSystemTempFile "output.ll" compile'
  where
    compile' fp handle = do
      T.hPutStrLn handle (cs $ ppllvm m)
      hClose handle
      let exe = "clang"
      let args = ["-Wno-override-module", "-lm", fp, "-o", exePath]
      callProcess exe args

renderModule :: AST.Module -> Text
renderModule = cs . ppllvm

renderParseError :: ParseErrorBundle Text Void -> Text
renderParseError = cs . errorBundlePretty
