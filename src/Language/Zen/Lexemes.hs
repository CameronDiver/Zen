module Language.Zen.Lexemes where

import           Control.Monad              (void)
import           Data.Char                  (ord)
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sp :: Parser ()
sp = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sp

symbol :: Text -> Parser Text
symbol = L.symbol sp

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquotes :: Parser a -> Parser a
dquotes = between (single '"') (single '"')

squotes :: Parser a -> Parser a
squotes = between (single '\'') (single '\'')

semi :: Parser ()
semi = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

equals :: Parser ()
equals = void $ symbol "="

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

boolean :: Parser Bool
boolean = true <|> false
  where
    true :: Parser Bool
    true = symbol "true" >> return True
    false = symbol "false" >> return False

rword :: Text -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [Text]
reservedWords = ["let", "true", "false", "if"]

stringLiteral :: Parser Text
stringLiteral = do
  content <- dquotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack (read ('"' : cs content ++ "\""))

charLiteral :: Parser Int
charLiteral =
  squotes $ (ord <$> satisfy (`notElem` ['\\', '\''])) <|> (single '\\' >> int)

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x
