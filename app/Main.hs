module Main where

import           Data.String.Conversions
import qualified Data.Text.IO            as T
import           LLVM.Pretty
import           Options.Applicative     hiding (action)
import           Text.Megaparsec.Error   (errorBundlePretty)
import           Text.Pretty.Simple

-- Uncomment if using Pretty custom instance
-- import           Data.Text.Prettyprint.Doc
-- import           Data.Text.Prettyprint.Doc.Render.Text
import           Language.Wind

data RunAction
  = AST
  | SemanticAST
  | LLVM
  | Compile

data Flag =
  Verbose
  deriving (Show)

data Options = Options { filename :: FilePath
                       , action :: RunAction
                       , flags :: [Flag]
                       }

optionsP :: Parser Options
optionsP =
  Options <$> strArgument (help "Source file" <> metavar "FILE") <*> runActionP <*>
  many flagP

runActionP :: Parser RunAction
runActionP =
  flag' AST (long "ast" <> short 'a' <> help "Print the AST after parsing") <|>
  flag'
    SemanticAST
    (long "sast" <> short 's' <> help "Print the semantically checked AST") <|>
  flag' LLVM (long "llvm" <> short 'l' <> help "Pring the LLVM IR") <|>
  pure Compile

flagP :: Parser Flag
flagP = flag' Verbose (long "verbose" <> help "Be verbose")

main :: IO ()
main = runOptions =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Run the wind compiler on the given file."

runOptions :: Options -> IO ()
runOptions (Options file action _) = do
  fileContent <- T.readFile file
  let eitherAst = generateAST file fileContent
  case eitherAst of
    Left e -> putStrLn $ errorBundlePretty e
    Right ast -> do
      let eitherSast = analyseAST ast
      case eitherSast of
        Left err -> print err
        Right sast -> do
          let llvm = generateLLVM sast
          case action of
            AST         -> pPrint ast -- putDoc $ pretty ast <> "\n"
            SemanticAST -> pPrint sast
            LLVM        -> (T.putStrLn . cs . ppllvm) llvm
            Compile     -> putStrLn "Cant compile yet"
