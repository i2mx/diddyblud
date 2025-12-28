module Parser.Expr where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List
import Lexer
import Parser.Combinators
import Parser.Core

type IdentifierName = String

data Expr
  = NumE Int
  | GEE Expr Expr
  | GTE Expr Expr
  | LTE Expr Expr
  | LEE Expr Expr
  | OrE Expr Expr
  | NotE Expr
  | UnaryPlusE Expr
  | UnaryMinusE Expr
  | AddressE Expr
  | DereferenceE Expr
  | AndE Expr Expr
  | EqualsE Expr Expr
  | NotEqualsE Expr Expr
  | AssignE Expr Expr
  | -- | SequenceE Expr Expr
    AddE Expr Expr
  | SubE Expr Expr
  | MulE Expr Expr
  | DivE Expr Expr
  | ModE Expr Expr
  | IdentE IdentifierName
  | -- | TemplateE [IdentifierName] Expr
    CallE Expr [Expr]
  | AccessE Expr IdentifierName
  | IndexE Expr Expr
  | BlockE [Stmt] Expr
  | IfElseE Expr Expr Expr
  | WhileElseE Expr (Maybe Expr) Stmt Expr
  deriving (Show)

data Stmt
  = BlockS [Stmt]
  | IfElse Expr Stmt (Maybe Stmt)
  | While Expr (Maybe Expr) Stmt
  | ReturnS (Maybe Expr)
  | ExprS Expr
  deriving (Show)

betweenParen :: Parser a -> Parser a
betweenParen p = do
  satisfy isLParen
  a <- p
  satisfy isRParen <?> "Expected cosing parenthesis"
  return a
  where
    isLParen (Token LParenT _) = True
    isLParen _ = False

    isRParen (Token RParenT _) = True
    isRParen _ = False

betweenBraces :: Parser a -> Parser a
betweenBraces p = do
  satisfy isLBrace
  a <- p
  satisfy isRBrace <?> "Expected closing brace"
  return a
  where
    isLBrace (Token LBraceT _) = True
    isLBrace _ = False

    isRBrace (Token RBraceT _) = True
    isRBrace _ = False

betweenBrackets :: Parser a -> Parser a
betweenBrackets p = do
  satisfy isLBracket
  a <- p
  satisfy isRBracket <?> "Expected closing bracket"
  return a
  where
    isLBracket (Token LBracketT _) = True
    isLBracket _ = False

    isRBracket (Token RBracketT _) = True
    isRBracket _ = False

-- i am so fucking sorry that i have to do this to you

exprMulOp :: Parser (Expr -> Expr -> Expr)
exprMulOp = satisfy isMul >> return MulE
  where
    isMul (Token MulT _) = True
    isMul _ = False

exprAddOp :: Parser (Expr -> Expr -> Expr)
exprAddOp = satisfy isAdd >> return AddE
  where
    isAdd (Token AddT _) = True
    isAdd _ = False

exprSubOp :: Parser (Expr -> Expr -> Expr)
exprSubOp = satisfy isSub >> return SubE
  where
    isSub (Token SubT _) = True
    isSub _ = False

-- matches an number literal and returns an expression
exprNum :: Parser Expr
exprNum = satisfy isNum >>= toE
  where
    toE (Token (NumT n) _) = return $ NumE n
    isNum (Token (NumT _) _) = True
    isNum _ = False

-- matches an identifier name and returns an expression
exprIdent :: Parser Expr
exprIdent = satisfy isIdent >>= toE
  where
    toE (Token (IdentT s) _) = return $ IdentE s
    isIdent (Token (IdentT _) _) = True
    isIdent _ = False

-- takes an identifier name and returns it
identNames :: Parser IdentifierName
identNames = satisfy isIdent >>= toS
  where
    toS (Token (IdentT s) _) = return s
    isIdent (Token (IdentT _) _) = True
    isIdent _ = False

-- matches ,
comma :: Parser ()
comma = void (satisfy isComma)
  where
    isComma (Token CommaT _) = True
    isComma _ = False

-- matches =
equals :: Parser ()
equals = void (satisfy isEquals)
  where
    isEquals (Token EqualsT _) = True
    isEquals _ = False

-- matches =>
fatArrow :: Parser ()
fatArrow = void (satisfy isFatArrow)
  where
    isFatArrow (Token FatArrowT _) = True
    isFatArrow _ = False

-- matches ?
placeholder :: Parser ()
placeholder = void (satisfy isPlaceholder)
  where
    isPlaceholder (Token PlaceholderT _) = True
    isPlaceholder _ = False

-- matches and
exprAndOp :: Parser (Expr -> Expr -> Expr)
exprAndOp = void (satisfy isAnd) >> return AndE
  where
    isAnd (Token AndT _) = True
    isAnd _ = False

-- matches or
exprOrOp :: Parser (Expr -> Expr -> Expr)
exprOrOp = void (satisfy isOr) >> return OrE
  where
    isOr (Token OrT _) = True
    isOr _ = False

-- matches <
exprLTOp :: Parser (Expr -> Expr -> Expr)
exprLTOp = void (satisfy isLT) >> return LTE
  where
    isLT (Token LTT _) = True
    isLT _ = False

-- matches >
exprGTOp :: Parser (Expr -> Expr -> Expr)
exprGTOp = void (satisfy isGT) >> return GTE
  where
    isGT (Token GTT _) = True
    isGT _ = False

-- matches <=
exprLEOp :: Parser (Expr -> Expr -> Expr)
exprLEOp = void (satisfy isLE) >> return LEE
  where
    isLE (Token LET _) = True
    isLE _ = False

-- matches >=
exprGEOp :: Parser (Expr -> Expr -> Expr)
exprGEOp = void (satisfy isGE) >> return GEE
  where
    isGE (Token GET _) = True
    isGE _ = False

-- matches ==
exprEqualsOp :: Parser (Expr -> Expr -> Expr)
exprEqualsOp = void (satisfy isEquals) >> return EqualsE
  where
    isEquals (Token EqualsT _) = True
    isEquals _ = False

exprAssignOp :: Parser (Expr -> Expr -> Expr)
exprAssignOp = void (satisfy f) >> return AssignE
  where
    f (Token AssignT _) = True
    f _ = False

whileP :: Parser ()
whileP = void (satisfy f)
  where
    f (Token WhileT _) = True
    f _ = False

ifP :: Parser ()
ifP = void (satisfy f)
  where
    f (Token IfT _) = True
    f _ = False

elseP :: Parser ()
elseP = void (satisfy f)
  where
    f (Token ElseT _) = True
    f _ = False

indentP :: Parser ()
indentP = void (satisfy f)
  where
    f (Token IndentT _) = True
    f _ = False

dedentP :: Parser ()
dedentP = void (satisfy f)
  where
    f (Token DedentT _) = True
    f _ = False

newLineP :: Parser ()
newLineP = void (satisfy f)
  where
    f (Token NewLineT _) = True
    f _ = False

colon :: Parser ()
colon = void (satisfy f)
  where
    f (Token ColonT _) = True
    f _ = False

-- exprSequenceOp :: Parser (Expr -> Expr -> Expr)
-- exprSequenceOp = void (satisfy f) >> return SequenceE
--   where
--     f (Token BarT _) = True
--     f _ = False

exprModOp :: Parser (Expr -> Expr -> Expr)
exprModOp = void (satisfy f) >> return ModE
  where
    f (Token ModT _) = True
    f _ = False

exprDivOp :: Parser (Expr -> Expr -> Expr)
exprDivOp = satisfy f >> return DivE
  where
    f (Token DivT _) = True
    f _ = False

returnP :: Parser ()
returnP = void (satisfy f)
  where
    f (Token DedentT _) = True
    f _ = False

semicolon :: Parser ()
semicolon = "Expected semicolon" <??> void (satisfy f)
  where
    f (Token SemicolonT _) = True
    f _ = False

exprNEQOp :: Parser (Expr -> Expr -> Expr)
exprNEQOp = satisfy f >> return NotEqualsE
  where
    f (Token NotEqualsT _) = True
    f _ = False

exprNotOp :: Parser (Expr -> Expr)
exprNotOp = satisfy f >> return NotE
  where
    f (Token NotT _) = True
    f _ = False

dotP :: Parser ()
dotP = void (satisfy f)
  where
    f (Token DotT _) = True
    f _ = False

exprUnaryPlus :: Parser (Expr -> Expr)
exprUnaryPlus = satisfy f >> return UnaryPlusE
  where
    f (Token AddT _) = True
    f _ = False

exprUnaryMinus :: Parser (Expr -> Expr)
exprUnaryMinus = satisfy f >> return UnaryMinusE
  where
    f (Token SubT _) = True
    f _ = False

exprDereference :: Parser (Expr -> Expr)
exprDereference = satisfy f >> return DereferenceE
  where
    f (Token MulT _) = True
    f _ = False

exprAddress :: Parser (Expr -> Expr)
exprAddress = satisfy f >> return AddressE
  where
    f (Token AddressT _) = True
    f _ = False

fnP :: Parser ()
fnP = void (satisfy f)
  where
    f (Token FnT _) = True
    f _ = False

number :: Parser Token
number = satisfy isNum
  where
    isNum (Token (NumT _) _) = True
    isNum _ = False
