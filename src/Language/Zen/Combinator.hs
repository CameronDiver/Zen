module Language.Zen.Combinator
  ( programParser
  ) where

import           Control.Monad.Combinators.Expr as E
import qualified Data.Text                      as T
import           Text.Megaparsec

import           Language.Zen.AST
import           Language.Zen.Lexemes

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
  BooleanLiteral <$> location <*> boolean <|>
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
callP =
  try
    (Call <$> (posToLocation <$> getSourcePos) <*> identifier <*>
     parens (exprP `sepBy` comma))

statementP :: Parser Statement
statementP =
  Expr <$> exprP <* semi <|> ifStatementP <|> whileStatementP <|> functionP

ifStatementP :: Parser Statement
ifStatementP = do
  loc <- posToLocation <$> getSourcePos
  predicate <- rword "if" >> exprP
  ifBody <- braces $ many statementP
  maybeElse <- option [] (rword "else" *> braces (many statementP))
  pure $ If loc predicate ifBody maybeElse

whileStatementP :: Parser Statement
whileStatementP = do
  loc <- posToLocation <$> getSourcePos
  predicate <- rword "while" *> exprP
  body <- braces $ many statementP
  pure $ While loc predicate body

functionP :: Parser Statement
functionP = do
  loc <- posToLocation <$> getSourcePos
  rword "fn"
  fnName <- identifier
  fnArgs <- parens $ sepBy functionArgP (symbol ",")
  returnType <- optional $ symbol "->" *> identifier
  body <- braces $ many statementP
  pure $ Function loc fnName fnArgs returnType body

functionArgP :: Parser FnArgument
functionArgP = do
  name <- identifier
  argType <- optional $ symbol ":" *> identifier
  pure $ FnArgument name argType

opTable :: [[E.Operator Parser Expr]]
opTable =
  [ [infixL Mul "*", infixL Div "/"]
  , [infixL Add "+", infixL Sub "-"]
  , [ infixL' Eq "=="
    , infixL' NEq "!="
    , infixL' LessEq "<="
    , infixL' GreaterEq ">="
    , infixL Less "<"
    , infixL Greater ">"
    ]
  , [InfixR $ Assign <$> location <* symbol "="]
  ]
  where
    infixL op sym = InfixL $ BinaryOp <$> location <*> (op <$ symbol sym)
    -- Primed infixL' is useful for operators which are prefixes of other operators
    infixL' op sym = InfixL $ BinaryOp <$> location <*> (op <$ operator sym)
    operator sym = lexeme $ try (symbol sym <* notFollowedBy opChar)
    opChar = oneOf ("+-" :: String)
    location = posToLocation <$> getSourcePos

posToLocation :: SourcePos -> Location
posToLocation (SourcePos sourceName sourceLine sourceCol) =
  Location (unPos sourceLine) (T.pack sourceName) (unPos sourceCol)
