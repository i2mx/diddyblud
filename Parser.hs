module Parser where

import Control.Applicative
import Control.Monad.State
import Data.Bifunctor
import Lexer
import Parser.Combinators
import Parser.Core
import Parser.Expr

-- Finally an expression parser that isn't ass

expr :: Parser Expr
expr = exprAssignment

-- TODO: think about whether or not i want inequality / equalitiy chaining where
-- these types of expressiosn where a <= b >= c ==> a <= b and b >= c

-- we can still get the unintuitive behaviour using (a == b) == c or something like that

exprAssignment = (exprLogicalOr <+> exprAssignOp) <|> exprLogicalOr

exprLogicalOr = (exprLogicalAnd <+> exprOrOp) <|> exprLogicalAnd

-- python behaviour
exprLogicalAnd = (exprComparative <+> exprAndOp) <|> exprComparative

exprComparative = (exprAdditive `compareChain` exprCompareOps) <|> exprAdditive
  where
    goCompare :: (Expr, [Expr]) -> (Expr -> Expr -> Expr, Expr) -> (Expr, [Expr])
    goCompare (x, cs) (f, y) = (y, f x y : cs)
    compareChain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
    compareChain pa pop = do
      a <- pa
      fbs <- many $ do
        op <- pop
        b <- pa
        return (op, b)
      let (_, comparisons) = foldl goCompare (a, []) fbs
      if null comparisons then return a else return $ foldl1 (flip AndE) comparisons
    exprCompareOps =
      exprEqualsOp
        <|> exprNEQOp
        <|> exprGTOp
        <|> exprGEOp
        <|> exprLTOp
        <|> exprLEOp

-- exprLogicalAnd = (exprEquality <+> exprAndOp) <|> exprEquality

-- exprEquality = (exprRelational <+> equalityOps) <|> exprRelational
--   where
--     equalityOps = exprEqualsOp <|> exprNEQOp

-- exprRelational = (exprAdditive <+> exprRelationalOps) <|> exprAdditive
--   where
--     exprRelationalOps = exprGTOp <|> exprGEOp <|> exprLTOp <|> exprLEOp

exprAdditive = (exprMultiplicative <+> additiveOps) <|> exprMultiplicative
  where
    additiveOps = exprAddOp <|> exprSubOp

exprMultiplicative = (exprUnary <+> multiplicativeOps) <|> exprUnary
  where
    multiplicativeOps = exprMulOp <|> exprDivOp <|> exprModOp

exprUnary = (unaryOps <*> exprUnary) <|> exprPrimary
  where
    unaryOps =
      exprNotOp
        <|> exprUnaryMinus
        <|> exprUnaryPlus
        <|> exprDereference
        <|> exprAddress

exprPrimary = do
  base <- exprBase
  postfixes base
  where
    exprBase =
      betweenParen expr
        <|> exprIdent
        <|> exprNum
        <|> exprBlock
        <|> exprIfElse
        <|> exprWhiteElse
    postfixes base =
      try arrayOp
        <|> try callOp
        <|> try accessOp
        <|> return base
      where
        arrayOp = do
          index <- betweenBrackets expr
          postfixes $ IndexE base index

        callOp = do
          arguments <- betweenParen argumentList
          postfixes $ CallE base arguments
          where
            argumentList = sepBy expr comma

        accessOp = do
          dotP
          field <- identNames
          postfixes $ AccessE base field

exprBlock = betweenBraces $ do
  statements <- many simpleStmt
  BlockE statements <$> expr

exprIfElse = do
  ifP
  condition <- betweenParen expr
  body <- expr
  elseP
  IfElseE condition body <$> expr

exprWhiteElse = do
  whileP
  condition <- betweenParen expr
  post <- Parser.Combinators.optional $ do
    colon
    betweenParen expr
  body <- simpleStmt
  elseP
  WhileElseE condition post body <$> expr

-- Statements
--  this is probably the nicest code in this whole project
-- ------------------------------------------------------------------------

simpleStmt = ifS <|> whileS <|> returnS <|> try blockS <|> exprS

blockS = BlockS <$> betweenBraces (many simpleStmt)

whileS =
  "white statement" <??> do
    whileP
    condition <- betweenParen expr
    post <- Parser.Combinators.optional $ do
      colon
      betweenParen expr
    While condition post <$> simpleStmt

-- TODO: if there is an if here and it gets an else and it cant
-- get the else then it gives up and just uses the if which is not ideal
ifS =
  "if statement" <??> do
    ifP
    condition <- betweenParen expr
    body <- simpleStmt
    elseClause <- Parser.Combinators.optional $ do
      elseP
      simpleStmt
    return $ IfElse condition body elseClause

returnS =
  "return statement" <??> do
    returnP
    expression <- Parser.Combinators.optional expr
    semicolon
    return $ ReturnS expression

exprS = ExprS <$> (expr <* semicolon)

-- end of Statements
-- this nice code now ends
-- ------------------------------------------------------------------------

runLexer text = evalState tokenize (initLexState text)

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
