import           Test.Tasty                          (TestTree, defaultMain,
                                                      testGroup)
import           Test.Tasty.Golden                   (findByExtension,
                                                      goldenVsString)
import           Test.Tasty.HUnit                    (assertEqual, testCase,
                                                      (@?=))

import           Data.String.Conversions
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as T
import           System.FilePath                     (replaceExtension,
                                                      takeBaseName)
import           Text.Megaparsec.Error               (errorBundlePretty)

import           Language.Zen
import           Language.Zen.SemanticAnalyser.Types

runFile :: FilePath -> IO Text
runFile infile = do
  prog <- T.readFile infile
  let parseTree = generateAST infile prog
  case parseTree of
    Left e -> return . cs $ errorBundlePretty e
    Right ast ->
      case analyseAST ast of
        Left e -> renderSemanticError e
        Right semanticTree ->
          T.pack <$> executeModuleWithArgs (generateLLVM semanticTree) ["TEST"]

runFile' :: FilePath -> IO (Bool, Text)
runFile' infile = do
  prog <- T.readFile infile
  let parseTree = generateAST infile prog
  case parseTree of
    Left e -> pure (False, cs $ errorBundlePretty e)
    Right ast ->
      case analyseAST ast of
        Left e -> (False, ) <$> renderSemanticError e
        Right semanticTree ->
          (True, ) <$>
          (T.pack <$> executeModuleWithArgs (generateLLVM semanticTree) ["TEST"])

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests =
  testGroup "all" <$> sequence [successTests, failTests, pure utilTests]

successTests :: IO TestTree
successTests = do
  files <- findByExtension [".z"] "test/data/pass"
  return $
    testGroup
      "Compile passing golden tests"
      [ goldenVsString (takeBaseName zFile) outputPath (cs <$> runFile zFile)
      | zFile <- files
      , let outputPath = replaceExtension zFile ".golden"
      ]

failTests :: IO TestTree
failTests = do
  files <- findByExtension [".z"] "test/data/fail"
  tests <-
    sequence [testCase (takeBaseName zFile) <$> failTest zFile | zFile <- files]
  pure $ testGroup "Compile failing tests" tests
  where
    failTest filename =
      assertEqual "Should fail" <$> (fst <$> runFile' filename) <*> pure False

utilTests :: TestTree
utilTests =
  testGroup
    "Utilities"
    [ testCase "Array types" $ do
        typeFromText "[int]" @?= Just (TyArray TyInt)
        typeFromText "[string]" @?= Just (TyArray TyString)
        typeFromText "[void]" @?= Just (TyArray TyVoid)
        typeFromText "[[int]]" @?= Just (TyArray $ TyArray TyInt)
    ]
