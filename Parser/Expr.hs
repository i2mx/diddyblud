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
  | AddE Expr Expr
  | SubE Expr Expr
  | MulE Expr Expr
  | IdentE IdentifierName
  | TemplateE [IdentifierName] Expr
  | ExpandE Expr [Expr]

-- custom formatting for Expr
pad = "|  "

display t (NumE n) = concat (replicate t pad) ++ show n
display t (IdentE s) = concat (replicate t pad) ++ s
display t (AddE a b) =
  concat (replicate t pad)
    ++ "Plus\n"
    ++ display (t + 1) a
    ++ "\n"
    ++ display (t + 1) b
display t (SubE a b) =
  concat (replicate t pad)
    ++ "Minus\n"
    ++ display (t + 1) a
    ++ "\n"
    ++ display (t + 1) b
display t (MulE a b) =
  concat (replicate t pad)
    ++ "Times\n"
    ++ display (t + 1) a
    ++ "\n"
    ++ display (t + 1) b
display t (TemplateE identifiers body) =
  concat (replicate t pad)
    ++ "Template ("
    ++ intercalate ", " identifiers
    ++ ") =>\n"
    ++ display (t + 1) body
display t (ExpandE body expressions) =
  concat (replicate t pad)
    ++ "Expand:\n"
    ++ display (t + 1) body
    ++ "\n"
    ++ concat (replicate t pad)
    ++ "With:\n"
    ++ intercalate "\n" (map (display (t + 1)) expressions)

instance Show Expr where
  show :: Expr -> String
  show = display 0

-- annoying basic token parsers

betweenParen :: Parser a -> Parser a
betweenParen p = do
  satisfy isLParen
  a <- p
  satisfy isRParen
  return a
  where
    isLParen (Token LParenT _) = True
    isLParen _ = False

    isRParen (Token RParenT _) = True
    isRParen _ = False

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
