module Parser where

import Control.Applicative
import Control.Monad.State
import Data.Bifunctor
import Lexer
import Parser.Combinators
import Parser.Core
import Parser.Expr

-- we define a parser of the form (lit, ...) => expr
exprAnonymousTemplate :: Parser Expr -> Parser Expr
exprAnonymousTemplate p = do
  identifierList <- betweenParen $ sepBy1 identNames comma
  fatArrow
  TemplateE identifierList <$> p

-- we define a parser of the form (expr)(expr, ...)(expr, ...)...
exprExpandAnonymousTemplate :: Parser Expr -> Parser Expr
exprExpandAnonymousTemplate p = do
  template <- betweenParen p
  manyExpressions <- some $ betweenParen $ sepBy1 p comma
  return $ foldl ExpandE template manyExpressions

-- define a parser of the form f(expr,...)(expr,...)...
exprExpandIdentifierTemplate :: Parser Expr -> Parser Expr
exprExpandIdentifierTemplate p = do
  template <- exprIdent
  manyExpressions <- some $ betweenParen $ sepBy1 p comma
  return $ foldl ExpandE template manyExpressions

expr :: Parser Expr
expr =
  exprAnonymousTemplate expr -- (x,y) => x
    <|> exprExpandAnonymousTemplate expr -- ((x,y) => x)(1) or ((x)=>(y)=>(x))(1)(2)
    <|> betweenParen expr -- (expr)
    <|> exprExpandIdentifierTemplate expr -- f(expr...)...
    <|> exprIdent -- x
    <|> exprNum -- 123
      <+> exprMulOp -- expr * expr
      <+> (exprAddOp <|> exprSubOp) -- expr + expr

eval :: Expr -> Int
eval (AddE a b) = (+) (eval a) (eval b)
eval (SubE a b) = (-) (eval a) (eval b)
eval (MulE a b) = (*) (eval a) (eval b)

runLexer = evalState tokenize (initLexState text)
  where
    text = "((x)=>(y)=>x)(1)(2) + f(1,2)(3)(4)"

run :: IO ()
run = do
  let x = runParser expr $ initParserState runLexer
  case x of
    (Left err) -> fail $ "Parse error" ++ show err
    (Right (result, state)) -> do
      print result
      print state
