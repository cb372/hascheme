module Parser
where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Datum = 
    Lex LexemeDatum 
  | Comp CompoundDatum
  deriving (Show)

data LexemeDatum = 
    BoolConst Bool 
  | Number Integer -- other number types not supported
  | Character Char
  | Str String 
  | Symbol String
  deriving (Show)

data CompoundDatum =
    List [Datum]
    -- vector, bytevector not supported
    -- abbreviations, quoting not supported
  deriving (Show)

type Parser = Parsec Void String

{- Lexing -}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

{- Parsing -}

datum :: Parser Datum
datum = do
  lex <- lexemeDatum -- <|> compoundDatum
  return (Lex lex)

lexemeDatum :: Parser LexemeDatum
lexemeDatum = bool -- <|> number <|> character <|> string <|> symbol

bool :: Parser LexemeDatum
bool = lexeme trueOrFalse
  where trueOrFalse :: Parser LexemeDatum
        trueOrFalse = (BoolConst True <$ string "#t")
                  <|> (BoolConst False <$ string "#f")
