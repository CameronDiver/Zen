module Language.Wind.Combinator
  ( programParser
  ) where

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec

import           Language.Wind.AST
import           Language.Wind.Lexemes

programParser :: Parser Program
programParser = between sp eof $ Program <$> many statementP

termP :: Parser Expr
termP =
  parens exprP <|> Literal <$> int <|> CharLiteral <$> charLiteral <|>
  StringLiteral <$> stringLiteral <|>
  Identifier <$> identifier <|>
  VarDeclaration <$> varDeclP

exprP :: Parser Expr
exprP = makeExprParser termP opTable

varDeclP :: Parser Expr
varDeclP = do
  _ <- rword "let"
  exprP

statementP :: Parser Statement
statementP = Expr <$> exprP <* semi

opTable :: [[Operator Parser Expr]]
opTable = [[infixL Add "+", infixL Sub "-"], [InfixR $ Assign <$ symbol "="]]
  where
    infixL op sym = InfixL $ BinaryOp op <$ operator sym
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("+-" :: [Char])
