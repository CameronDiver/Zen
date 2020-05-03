module Language.Zen
  ( generateAST
  , generateLLVM
  , analyseAST
  , executeModule
  , compileModule
  , renderModule
  , renderParseError
  , renderSemanticError
  , Location
  ) where

import           Data.String.Conversions
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Void
import qualified LLVM.AST                              as AST
import           LLVM.Pretty
import           Text.Megaparsec                       (runParser)
import           Text.Megaparsec.Error                 (ParseErrorBundle,
                                                        errorBundlePretty)

import           System.IO
import           System.IO.Temp
import           System.Process

import           Language.Zen.AST
import           Language.Zen.CodeGen
import           Language.Zen.Combinator
import           Language.Zen.SemanticAnalyser
import           Language.Zen.SemanticAnalyser.AST
import           Language.Zen.SemanticAnalyser.Error

generateAST :: String -> Text -> Either (ParseErrorBundle Text Void) Program
generateAST = runParser programParser

analyseAST :: Program -> Either SemanticError SAProgram
analyseAST = checkProgram

generateLLVM :: SAProgram -> AST.Module
generateLLVM = codegenProgram

-- TODO: Implement this with a JIT
executeModule :: AST.Module -> IO String
executeModule m = withSystemTempFile "zen-exe" execute'
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

renderSemanticError :: SemanticError -> IO Text
renderSemanticError e = do
  source <- renderSourcefileLocation $ loc e
  return $
    T.concat
      [ renderStrict $
        layoutPretty defaultLayoutOptions ("Error:" <+> pretty e <> "\n")
      , "\n"
      , source
      ]

-- TODO: This opens the file for reading again, but we could
-- just pass it in from main
renderSourcefileLocation :: Location -> IO Text
renderSourcefileLocation (Location lineno file col) = do
  content <- T.readFile $ T.unpack file
  let l = T.lines content !! (lineno - 1)
  return $ T.concat [l, "\n", T.pack (replicate (col - 1) ' '), "^"]
