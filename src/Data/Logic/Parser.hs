module Data.Logic.Parser where

import Data.Logic.Classical ( BOp(Or, And), Expr(..), UOp(Not) )
import Text.ParserCombinators.Parsec
    ( alphaNum,
      char,
      letter,
      spaces,
      string,
      (<?>),
      many,
      parse,
      ParseError,
      Parser )
import Text.ParserCombinators.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocLeft),
      Operator(Prefix, Infix) )

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

-- note this doesn't currently handle whitespace
expr :: Parser Expr
expr = buildExpressionParser table parseVar <?> "expression"

parseVar :: Parser Expr
parseVar = do
  c <- letter
  cs <- many alphaNum
  return $ Var (c : cs)

parens :: Parser a -> Parser a
parens p = do
  char '(' >> spaces
  x <- p
  _ <- spaces >> char ')'
  return x

table :: [[Operator Char st Expr]]
table =
  [ [prefix "~" (Unary Not)],
    [binary "&" (Binary And) AssocLeft],
    [binary "|" (Binary Or) AssocLeft]
  ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Char st a
binary name fun = Infix (do _ <- string name; return fun)

prefix :: String -> (a -> a) -> Operator Char st a
prefix name fun = Prefix (do _ <- string name; return fun)

-- >>> parseExpr "ϕ|ψ|ψ"
-- Right ((ϕ | ψ) | ψ)
