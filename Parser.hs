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
      <+> exprModOp
      <+> (exprLEOp <|> exprLTOp <|> exprGEOp <|> exprGTOp) -- expr <= expr
      <+> exprEqualsOp -- expr == expr
      <+> exprAndOp -- expr and expr
      <+> exprOrOp -- expr or expr
      <+> exprAssignOp -- expr or expr
      <+> exprSequenceOp -- expr or expr

replace e i (AddE a b) = AddE (replace e i a) (replace e i b)
replace e i (SubE a b) = SubE (replace e i a) (replace e i b)
replace e i (MulE a b) = MulE (replace e i a) (replace e i b)
replace e i (TemplateE x body) = TemplateE x (replace e i body)
replace e i (IdentE s) = if i == s then e else IdentE s
replace e i (NumE n) = NumE n
replace e i (ExpandE body exprs) = ExpandE (replace e i body) (replace e i <$> exprs)

substitute [] [] body = body
substitute (e : es) (i : is) body = substitute es is (replace e i body)

eval :: Expr -> Expr
eval (NumE n) = NumE n
eval (AddE a b) = add (eval a) (eval b)
  where
    add (NumE a) (NumE b) = NumE (a + b)
    add a b = AddE a b
eval (SubE a b) = sub (eval a) (eval b)
  where
    sub (NumE a) (NumE b) = NumE (a - b)
    sub a b = SubE a b
eval (MulE a b) = mul (eval a) (eval b)
  where
    mul (NumE a) (NumE b) = NumE (a * b)
    mul a b = MulE a b
eval (ExpandE (TemplateE identifiers body) expressions) =
  eval $ substitute ee identifiers eb
  where
    ee = map eval expressions
    eb = eval body
eval (ExpandE body expressions) = tryAgain eb ee
  where
    ee = map eval expressions
    eb = eval body
    tryAgain (TemplateE ids bdy) expressions = eval $ substitute expressions ids (eval bdy) -- do i need to simplify here?
    tryAgain body expressions = ExpandE body expressions
eval (TemplateE [] body) = body
eval (TemplateE ids body) = TemplateE ids (eval body)
eval r = r

runLexer text = evalState tokenize (initLexState text)

-- Statements
data Stmt
  = BlockS [Stmt]
  | ExprS Expr
  | While Expr Stmt
  | IfElse Expr Stmt (Maybe Stmt)
  deriving (Show)

simpleStmt = ifS <|> whileS <|> blockS <|> exprS

blockS = BlockS <$> betweenBraces (insideBlock simpleStmt)

exprS = ExprS <$> expr

whileS = do
  whileP
  condition <- betweenParen expr
  post <- Parser.Combinators.optional $ do
    colon
    Parser.Combinators.optional newLineP
    betweenParen expr
  body <- simpleStmt
  return $ case post of
    Nothing -> While condition body
    Just something -> While condition (BlockS [body, ExprS something])

ifS = do
  ifP
  condition <- betweenParen expr
  Parser.Combinators.optional newLineP
  body <- simpleStmt
  Parser.Combinators.optional newLineP
  elseClause <- Parser.Combinators.optional $ do
    elseP
    Parser.Combinators.optional newLineP
    simpleStmt
  return $ IfElse condition body elseClause

insideBlock something = many newLineP >> sepBy something (some newLineP) <* many newLineP

run :: IO ()
run = do
  text <- readFile "example"
  let lala = runLexer text
  mapM_ print lala

  let x = runParser simpleStmt $ initParserState (runLexer text)
  case x of
    (Left err) -> fail $ "Parse error" ++ show err
    (Right (result, state)) -> do
      print result
      putStrLn $ replicate 40 '-'
      -- print $ eval result

      mapM_ print $ Parser.Core.input state
