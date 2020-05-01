module Language.Wind where

import           Data.Text
import           Data.Void
import qualified LLVM.AST                           as AST
import           Text.Megaparsec                    (runParser)
import           Text.Megaparsec.Error              (ParseErrorBundle)

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
