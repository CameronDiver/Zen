module Language.Wind.Combinator
  ( programParser
  ) where

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec

import           Language.Wind.Lexemes
import           Language.Wind.Parser

programParser :: Parser Program
programParser = between sp eof $ do Program <$> many statementP

termP :: Parser Expr
termP =
  parens exprP <|> Literal <$> int <|> CharLiteral <$> charLiteral <|>
  StringLiteral <$> stringLiteral <|>
  Identifier <$> identifier

exprP :: Parser Expr
exprP = makeExprParser termP opTable

exprMaybeP :: Parser Expr
exprMaybeP = option NoExpr exprP

varDeclP :: Parser Expr
varDeclP = do
  _ <- rword "let"
  exprP

statementP :: Parser Statement
statementP = Expr <$> exprP <* semi <|> VarDeclaration <$> varDeclP <* semi

opTable :: [[Operator Parser Expr]]
opTable = [[infixL Add "+", infixL Sub "-"], [InfixR $ Assign <$ symbol "="]]
  where
    infixL op sym = InfixL $ BinaryOp op <$ operator sym
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("+-" :: [Char])
