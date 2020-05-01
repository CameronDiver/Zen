module Main where

import           Control.Monad
import           Data.String.Conversions
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           LLVM.Pretty
import           Options.Applicative     hiding (action)
import           System.Exit
import           Text.Megaparsec.Error   (errorBundlePretty)
import           Text.Pretty.Simple

import           Language.Wind

data RunAction
  = AST
  | SemanticAST
  | LLVM
  | Compile
  | Execute
  deriving (Eq)

data Flag =
  Verbose
  deriving (Show)

data Options
  = Options
      { filename :: FilePath
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
  flag'
    Compile
    (long "compile" <> short 'c' <> help "Compile the file to a native binary") <|>
  pure Execute

flagP :: Parser Flag
flagP = flag' Verbose (long "verbose" <> help "Be verbose")

main :: IO ()
main = runOptions =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Compile and execute a wind source file."

runOptions :: Options -> IO ()
runOptions (Options file action _) = do
  fileContent <- T.readFile file
  let eitherAst = generateAST file fileContent
  case eitherAst of
    Left e -> putStrLn $ errorBundlePretty e
    Right ast -> do
      when
        (action == AST)
        (do pPrint ast
            exitSuccess)
      let eitherSast = analyseAST ast
      case eitherSast of
        Left err -> print err
        Right sast -> do
          let llvm = generateLLVM sast
          case action of
            AST         -> pPrint ast -- putDoc $ pretty ast <> "\n"
            SemanticAST -> pPrint sast
            LLVM        -> (T.putStrLn . cs . ppllvm) llvm
            Compile     -> compileModule llvm $ exePath file
            Execute     -> executeModule llvm >>= putStr
  where
    exePath path =
      T.unpack $ T.intercalate "." $ init $ T.splitOn "." (T.pack path)
