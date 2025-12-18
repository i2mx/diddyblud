module Parser.Core where

import Control.Applicative
import Data.Bifunctor
import Lexer

type Pos = Int

data Error = ParseError {message :: String, furthest :: Parser.Core.Pos}
  deriving (Show)

data ParserState = ParserState {input :: [Token], pos :: Parser.Core.Pos}
  deriving (Show)

newtype Parser a = Parser {runParser :: ParserState -> Either Error (a, ParserState)}

-- the boring type class instancing

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ fmap (first f) . runParser p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ Right . (x,)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser px = Parser $ \input -> do
    (f, rest) <- pf input
    (x, final) <- px rest
    return (f x, final)

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser px >>= fp = Parser $ \input -> do
    (x, rest) <- px input
    (y, final) <- runParser (fp x) rest
    return (y, final)

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser $ Left . ParseError msg . Parser.Core.pos

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ Left . ParseError "empty alternative" . Parser.Core.pos
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser px <|> Parser py = Parser $ \input ->
    case px input of
      e1@(Left (ParseError _ dist1))
        | dist1 == Parser.Core.pos input -> py input
        | otherwise -> case py input of
            e2@(Left (ParseError _ dist2)) -> if dist1 >= dist2 then e1 else e2
            success -> success
      success -> success

-- some less boring things
(<?>) :: Parser a -> String -> Parser a
Parser p <?> msg = Parser $ \s ->
  case p s of
    Left (ParseError _ pos) -> Left (ParseError msg pos)
    success -> success

-- got this idea from parsec heheh
try :: Parser a -> Parser a
try (Parser p) = Parser $ \s ->
  case p s of
    Left (ParseError msg _) -> Left (ParseError msg (Parser.Core.pos s))
    success -> success

-- basic parsers that we'll use to create more interesting parsers later

takeToken :: Parser Token
takeToken = Parser $ \(ParserState input pos) ->
  case input of
    [] -> Left $ ParseError "unexpected end of file" pos
    (t : ts) -> Right (t, ParserState ts (pos + 1))

finished :: Parser ()
finished = Parser $ \(ParserState input pos) ->
  case input of
    [] -> Right ((), ParserState [] pos)
    _ -> Left $ ParseError "expected end of file" pos

-- only consumes the next token if it matches
takeIf :: (Token -> Bool) -> Parser Token
takeIf p = do
  t <- takeToken
  if p t then return t else empty

satisfy :: (Token -> Bool) -> Parser Token
satisfy = takeIf

choice :: [Parser a] -> Parser a
choice = asum
