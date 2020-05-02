module Language.Wind.Combinator
  ( programParser
  ) where

import           Control.Monad.Combinators.Expr as E
import qualified Data.Text                      as T
import           Text.Megaparsec

import           Language.Wind.AST
import           Language.Wind.Lexemes

programParser :: Parser Program
programParser = between sp eof $ Program <$> many statementP

termP :: Parser Expr
termP =
  parens exprP <|>
  ---------------------
  callP <|>
  ---------------------
  varDeclP <|>
  ---------------------
  FloatLiteral <$> location <*> try float <|>
  ---------------------
  Literal <$> location <*> int <|>
  ---------------------
  CharLiteral <$> location <*> charLiteral <|>
  ---------------------
  StringLiteral <$> location <*> stringLiteral <|>
  ---------------------
  Identifier <$> location <*> identifier
  where
    location = posToLocation <$> getSourcePos

exprP :: Parser Expr
exprP = makeExprParser termP opTable

varDeclP :: Parser Expr
varDeclP = do
  _ <- rword "let"
  location <- posToLocation <$> getSourcePos
  VarDeclaration location <$> exprP

callP :: Parser Expr
callP = try (Call <$> (posToLocation <$> getSourcePos ) <*> identifier <*> parens (exprP `sepBy` comma))

statementP :: Parser Statement
statementP = Expr <$> exprP <* semi

opTable :: [[E.Operator Parser Expr]]
opTable = [[infixL Add "+", infixL Sub "-"], [InfixR $ Assign <$> location <* symbol "="]]
  where
    infixL op sym = InfixL $ BinaryOp <$> location <*> (op <$ operator sym)
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("+-" :: [Char])
    location = posToLocation <$> getSourcePos

posToLocation :: SourcePos -> Location
posToLocation (SourcePos sourceName sourceLine sourceCol) = Location (unPos sourceLine) (T.pack sourceName) (unPos sourceCol)
