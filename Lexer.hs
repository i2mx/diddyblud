-- i think both lexers and parsers can be generalized to Scanners
-- it would be awesome to be able to turn this into a DFA that would be so fucking cool

module Lexer where

import Control.Monad.State
import Data.Char
import Data.Functor
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
  | EOFT -- literals
  | IdentT String
  | NumT Int
  | TrueT
  | FalseT
  | AddT -- operators and delimiters
  | SubT
  | MulT
  | ModT
  | LTT
  | LET
  | GTT
  | GET
  | AndT
  | OrT
  | LParenT
  | RParenT
  | LBraceT
  | RBraceT
  | CommaT
  | SemicolonT
  | BarT
  | AssignT
  | EqualsT
  | FatArrowT -- arrows and pipes and templates
  | PipeT
  | PlaceholderT
  | TemplateT
  | IndentT -- python :sob:
  | NodentT
  | DedentT
  | NewLineT
  | ColonT
  | DefT -- keywords
  | WhileT
  | IfT
  | ElifT
  | ElseT
  | ReturnT
  | LetT
  | VarT
  deriving (Show)

data Token = Token {kind :: TokenKind, span :: Span}
  deriving (Show)

type Indent = Int

data LexState = LexState {input :: String, pos :: Pos, indent :: [Indent]}
  deriving (Show)

type Lexer a = State LexState a

advance :: Pos -> Char -> Pos
advance (Pos l c) '\n' = Pos (l + 1) 1
advance (Pos l c) char = Pos l (c + 1)

advanceBy :: Pos -> String -> Pos
advanceBy = foldl advance

getNextChar :: Lexer (Maybe Char)
getNextChar = state $ \(LexState input pos indents) ->
  case input of
    [] -> (Nothing, LexState input pos indents)
    (x : xs) -> (Just x, LexState xs (advance pos x) indents)

peekNextChar :: Lexer (Maybe Char)
peekNextChar = state $ \(LexState input pos indents) ->
  case input of
    [] -> (Nothing, LexState input pos indents)
    (x : xs) -> (Just x, LexState input pos indents)

skipWhitespace :: Lexer ()
skipWhitespace = do
  mc <- peekNextChar
  case mc of
    Just c | c == ' ' -> getNextChar >> skipWhitespace
    _ -> return ()

takeSpaces :: Lexer String
takeSpaces = state $ \(LexState input pos indents) ->
  case input of
    [] -> ("", LexState input pos indents)
    xs -> (spaces, LexState rest (advanceBy pos spaces) indents)
      where
        (spaces, rest) = Prelude.span (== ' ') input

getNextToken :: Lexer Token
getNextToken = do
  skipWhitespace
  startPos <- gets pos
  mc <- getNextChar
  case mc of
    Nothing -> Token EOFT (Span startPos startPos) <$ skipWhitespace
    -- Just ' ' -> do
    --   padding <- takeSpaces
    --   indentation <- gets indent
    --   let indentLevel = length padding + 1
    --   if indentLevel == head indentation
    --     then return (Token NodentT (Span startPos (advanceBy startPos padding)))
    --     else
    --       if indentLevel >= head indentation
    --         then
    --           modify (\(LexState a b c) -> LexState a b (indentLevel : c))
    --             >> return (Token IndentT (Span startPos (advanceBy startPos padding)))
    --         else
    --           modify (\(LexState a b c) -> LexState a b (tail c))
    --             >> return (Token DedentT (Span startPos (advanceBy startPos padding)))
    Just ':' -> Token ColonT (Span startPos (advance startPos '+')) <$ skipWhitespace
    Just '+' -> Token AddT (Span startPos (advance startPos '+')) <$ skipWhitespace
    Just '-' -> Token SubT (Span startPos (advance startPos '-')) <$ skipWhitespace
    Just '*' -> Token MulT (Span startPos (advance startPos '*')) <$ skipWhitespace
    Just '(' -> Token LParenT (Span startPos (advance startPos '(')) <$ skipWhitespace
    Just ')' -> Token RParenT (Span startPos (advance startPos ')')) <$ skipWhitespace
    Just '{' -> Token LBraceT (Span startPos (advance startPos '(')) <$ skipWhitespace
    Just '}' -> Token RBraceT (Span startPos (advance startPos ')')) <$ skipWhitespace
    Just ',' -> Token CommaT (Span startPos (advance startPos ',')) <$ skipWhitespace
    Just '?' -> Token PlaceholderT (Span startPos (advance startPos '?')) <$ skipWhitespace
    Just '%' -> Token ModT (Span startPos (advance startPos '?')) <$ skipWhitespace
    Just '>' -> do
      md <- peekNextChar
      case md of
        Just '=' ->
          getNextChar >> Token GET (Span startPos (advanceBy startPos "==")) <$ skipWhitespace
        _ -> Token GTT (Span startPos (advance startPos '=')) <$ skipWhitespace
    Just '<' -> do
      md <- peekNextChar
      case md of
        Just '=' ->
          getNextChar >> Token LET (Span startPos (advanceBy startPos "==")) <$ skipWhitespace
        _ -> Token LTT (Span startPos (advance startPos '=')) <$ skipWhitespace
    Just '=' -> do
      md <- peekNextChar
      case md of
        Just '=' ->
          getNextChar >> Token EqualsT (Span startPos (advanceBy startPos "==")) <$ skipWhitespace
        Just '>' ->
          getNextChar >> Token FatArrowT (Span startPos (advanceBy startPos "=>")) <$ skipWhitespace
        _ -> Token AssignT (Span startPos (advance startPos '=')) <$ skipWhitespace
    Just '|' -> do
      md <- peekNextChar
      case md of
        Just '>' ->
          getNextChar >> Token PipeT (Span startPos (advanceBy startPos "|>")) <$ skipWhitespace
        _ -> Token BarT (Span startPos (advance startPos '|')) <$ skipWhitespace
    Just '\n' -> return $ Token NewLineT (Span startPos (advance startPos '\n')) -- pmo
    Just ';' -> Token SemicolonT (Span startPos (advance startPos ';')) <$ skipWhitespace -- pmo
    Just c
      | isNumber c -> lexNumber c startPos <* skipWhitespace
      | isIdentAlpha c -> lexWord c startPos <* skipWhitespace
      | otherwise -> Token IllegalT (Span startPos (advance startPos c)) <$ skipWhitespace

getTokenKind :: String -> TokenKind
getTokenKind "template" = TemplateT
getTokenKind "while" = WhileT
getTokenKind "true" = TrueT
getTokenKind "false" = FalseT
getTokenKind "and" = AndT
getTokenKind "or" = OrT
getTokenKind "def" = DefT
getTokenKind "if" = IfT
getTokenKind "elif" = ElifT
getTokenKind "else" = ElseT
getTokenKind "return" = ReturnT
getTokenKind "let" = LetT
getTokenKind "var" = VarT
getTokenKind "mod" = ModT
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

notNodent (Token NodentT _) = False
notNodent _ = True

tokenize :: Lexer [Token]
tokenize =
  filter notNodent
    <$> do
      tk <- getNextToken
      case kind tk of
        EOFT -> return []
        _ -> (tk :) <$> tokenize

initLexState :: String -> LexState
initLexState input = LexState input initialPos [0]
  where
    initialPos = Pos 1 1

-- run = kind <$> evalState tokenize (initLexState "Add(x,y) = x + y\nIncrement=Add(?,1)\nIncrement(5)\n5 |> Increment")
