module Parser.Combinators where

import Control.Applicative
import Lexer
import Parser.Core

infixl 2 <+>

(<+>) = chain1

(<??>) = flip (<?>)

-- a parser for `a sep a sep a sep`
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

optional :: Parser a -> Parser (Maybe a)
optional pa = Just <$> pa <|> return Nothing

between :: Parser open -> Parser close -> Parser a -> Parser a
between popen pclose pa = do
  popen
  a <- pa
  pclose
  return a

chain1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chain1 pa pop = do
  a <- pa
  fbs <- many $ do
    op <- pop
    b <- pa
    return (op, b)
  return $ foldl (\x (f, y) -> x `f` y) a fbs

initParserState :: [Token] -> ParserState
initParserState = flip ParserState 0
