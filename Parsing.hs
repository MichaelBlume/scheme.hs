module Parsing (parseLispVal) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Control.Applicative ((<*))

import LispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseLispVal = oneOfP [ parseAtom, parseList
                      , parseString, parseString, parseNumber]

oneOfP = foldr1 (<|>)

spaces = skipMany1 space

parseString = liftM String $ char '"' >> many parseChar <* char '"'

parseChar = noneOf "\"\\" <|> parseEscaped

quoteData = [('n', '\n'), ('r', '\r'), ('t', '\t'), ('\\', '\\'), ('"', '"')]

parseEscaped = char '\\' >> (oneOfP $ map parseQuotePair quoteData)

parseQuotePair (a,b) = char a >> return b

parseNumber = liftM (Number . read) $ many1 (digit <|> char '.')

parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseListStart = char '(' >> sepBy parseLispVal spaces

parseListEnd = char ')' >> return List

parseDottedListEnd = do
  result <- spaces >> char '.' >> spaces >> parseLispVal
  char ')'
  return $ (flip DottedList) result

parseList = do
  listpart <- parseListStart
  constructor <- parseListEnd <|> parseDottedListEnd
  return $ constructor listpart
