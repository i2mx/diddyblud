-- i think both lexers and parsers can be generalized to Scanners
-- it would be awesome to be able to turn this into a DFA that would be so fucking cool

module Lexer where

import Control.Monad.State
import Data.Char
import Text.Printf

type Line = Int

type Column = Int

data Pos = Pos {line :: Line, column :: Column}

instance Show Pos where
  show :: Pos -> String
  show (Pos l c) = printf "%d:%d" l c

data Span = Span {start :: Pos, end :: Pos}

instance Show Span where
  show :: Span -> String
  show (Span p q) = show p ++ "-" ++ show q

data TokenKind
  = IllegalT
  | EOFT
  | IdentT String
  | AddT
  | SubT
  | MulT
  | LParenT
  | RParenT
  | CommaT
  | EqualsT
  | FatArrowT
  | PipeT
  | PlaceholderT
  | NumT Int
  | TemplateT
  deriving (Show)

data Token = Token {kind :: TokenKind, span :: Span}
  deriving (Show)

data LexState = LexState {input :: String, pos :: Pos}
  deriving (Show)

type Lexer a = State LexState a

advance :: Pos -> Char -> Pos
advance (Pos l c) '\n' = Pos (l + 1) 1
advance (Pos l c) char = Pos l (c + 1)

advanceBy :: Pos -> String -> Pos
advanceBy = foldl advance

getNextChar :: Lexer (Maybe Char)
getNextChar = state $ \(LexState input pos) ->
  case input of
    [] -> (Nothing, LexState input pos)
    (x : xs) -> (Just x, LexState xs (advance pos x))

peekNextChar :: Lexer (Maybe Char)
peekNextChar = state $ \(LexState input pos) ->
  case input of
    [] -> (Nothing, LexState input pos)
    (x : xs) -> (Just x, LexState input pos)

skipWhitespace :: Lexer ()
skipWhitespace = do
  mc <- peekNextChar
  case mc of
    Just c | isSpace c -> getNextChar >> skipWhitespace
    _ -> return ()

getNextToken :: Lexer Token
getNextToken = do
  skipWhitespace
  startPos <- gets pos
  mc <- getNextChar
  case mc of
    Nothing -> return $ Token EOFT (Span startPos startPos)
    Just '+' -> return $ Token AddT (Span startPos (advance startPos '+'))
    Just '-' -> return $ Token SubT (Span startPos (advance startPos '-'))
    Just '*' -> return $ Token MulT (Span startPos (advance startPos '*'))
    Just '(' -> return $ Token LParenT (Span startPos (advance startPos '('))
    Just ')' -> return $ Token RParenT (Span startPos (advance startPos ')'))
    Just ',' -> return $ Token CommaT (Span startPos (advance startPos ','))
    Just '?' -> return $ Token PlaceholderT (Span startPos (advance startPos '?'))
    Just '=' -> do
      md <- peekNextChar
      case md of
        Just '>' ->
          getNextChar >> return (Token FatArrowT (Span startPos (advanceBy startPos "=>")))
        _ -> return $ Token EqualsT (Span startPos (advance startPos '='))
    Just '|' -> do
      md <- peekNextChar
      case md of
        Just '>' ->
          getNextChar >> return (Token PipeT (Span startPos (advanceBy startPos "|>")))
        _ -> return $ Token IllegalT (Span startPos (advance startPos '|'))
    Just c
      | isNumber c -> lexNumber c startPos
      | isIdentAlpha c -> lexWord c startPos
      | otherwise -> return $ Token IllegalT (Span startPos (advance startPos c))

getTokenKind :: String -> TokenKind
getTokenKind "template" = TemplateT
getTokenKind str = IdentT str

lexNumber :: Char -> Pos -> Lexer Token
lexNumber c startPos = do
  rest <- collectWhile isNumber
  let text = c : rest
      endPos = advanceBy startPos text
  return $ Token (NumT (read text)) (Span startPos endPos)

lexWord :: Char -> Pos -> Lexer Token
lexWord c startPos = do
  rest <- collectWhile isIdentAlphaNum
  let text = c : rest
      endPos = advanceBy startPos text
  return $ Token (getTokenKind text) (Span startPos endPos)

collectWhile :: (Char -> Bool) -> Lexer String
collectWhile pred = do
  mc <- peekNextChar
  case mc of
    Just c | pred c -> getNextChar >> (c :) <$> collectWhile pred
    _ -> return []

isIdentAlpha :: Char -> Bool
isIdentAlpha '_' = True
isIdentAlpha c = isAlpha c

isIdentAlphaNum :: Char -> Bool
isIdentAlphaNum '_' = True
isIdentAlphaNum c = isAlphaNum c

tokenize :: Lexer [Token]
tokenize = do
  tk <- getNextToken
  case kind tk of
    EOFT -> return []
    _ -> (tk :) <$> tokenize

initLexState :: String -> LexState
initLexState input = LexState input initialPos
  where
    initialPos = Pos 1 1

-- run = kind <$> evalState tokenize (initLexState "Add(x,y) = x + y\nIncrement=Add(?,1)\nIncrement(5)\n5 |> Increment")
