import           Test.Tasty              (TestTree, defaultMain, testGroup)
import           Test.Tasty.Golden       (findByExtension, goldenVsString)

import           Data.String.Conversions
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           System.FilePath         (replaceExtension, takeBaseName)
import           Text.Megaparsec.Error   (errorBundlePretty)

import           Language.Zen

runFile :: FilePath -> IO Text
runFile infile = do
  prog <- T.readFile infile
  let parseTree = generateAST infile prog
  case parseTree of
    Left e -> return . cs $ errorBundlePretty e
    Right ast ->
      case analyseAST ast of
        Left e -> return $ T.pack $ show e
        Right semanticTree ->
          T.pack <$> executeModule (generateLLVM semanticTree)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  wFiles <- findByExtension [".z"] "test/data/pass"
  return $
    testGroup
      "Compile golden tests"
      [ goldenVsString (takeBaseName wFile) outputPath (cs <$> runFile wFile)
      | wFile <- wFiles
      , let outputPath = replaceExtension wFile ".golden"
      ]
