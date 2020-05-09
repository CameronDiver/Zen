module Language.Zen.Combinator
  ( programParser
  ) where

import           Control.Monad.Combinators.Expr as E
import qualified Data.Text                      as T
import           Text.Megaparsec

import           Language.Zen.AST
import           Language.Zen.Lexemes

programParser :: Parser Program
programParser = between sp eof $ Program <$> many topLevelParser

topLevelParser :: Parser TopLevel
topLevelParser = (Statement <$> statementP) <|> (Fn <$> functionP)

termP :: Parser Expr
termP =
  parens exprP <|>
  ---------------------
  callP <|>
  ---------------------
  varDeclP <|>
  ---------------------
  returnP <|>
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
    location = getLocation

exprP :: Parser Expr
exprP = makeExprParser termP opTable

varDeclP :: Parser Expr
varDeclP = do
  _ <- rword "let"
  location <- getLocation
  VarDeclaration location <$> exprP

callP :: Parser Expr
callP =
  try (Call <$> getLocation <*> identifier <*> parens (exprP `sepBy` comma))

returnP :: Parser Expr
returnP
  -- Do the order like this to get the location correct in
  -- error messages
 = do
  _ <- rword "return"
  Return <$> getLocation <*> optional exprP

statementP :: Parser Statement
statementP = Expr <$> exprP <* semi <|> ifStatementP <|> whileStatementP

ifStatementP :: Parser Statement
ifStatementP = do
  loc <- getLocation
  predicate <- rword "if" >> exprP
  ifBody <- braces $ many statementP
  maybeElse <- option [] (rword "else" *> braces (many statementP))
  pure $ If loc predicate ifBody maybeElse

whileStatementP :: Parser Statement
whileStatementP = do
  loc <- getLocation
  predicate <- rword "while" >> exprP
  body <- braces $ many statementP
  pure $ While loc predicate body

functionP :: Parser FunctionDef
functionP = do
  loc <- getLocation
  name <- rword "fn" *> identifier
  args <- parens (sepBy argP comma)
  _ <- symbol "->"
  rloc <- getLocation
  ret <- identifier
  body <- braces $ many statementP
  pure $ FunctionDef loc name args ret rloc body

argP :: Parser FunctionArg
argP = do
  name <- identifier
  _ <- symbol ":"
  loc <- getLocation
  FunctionArg loc name <$> identifier

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
    location = getLocation

getLocation :: Parser Location
getLocation = do
  (SourcePos sourceName sourceLine sourceCol) <- getSourcePos
  pure $ Location (unPos sourceLine) (T.pack sourceName) (unPos sourceCol)
