module Data.Logic.Classical where

import Text.ParserCombinators.ReadP

newtype Var = Var Char deriving (Show,Eq)

-- This datatype characterizes the set of wffs of classical logic
-- I'm factoring out binary and unary operators to make this more readily extensible.
data Expr = Variable Var | Unary UOp Expr | Binary BOp Expr Expr deriving (Show)

data BOp = And | Or deriving Show
data UOp = Not deriving Show

-- Extremely primitive parser

variable :: ReadP Expr
variable = do
  s <- choice (map char ['a'..'z'])
  return $ Variable $ Var s

conjunction :: ReadP Expr
conjunction = do
  a <- variable
  _ <- char '&'
  Binary And a <$> formula

disjunction :: ReadP Expr
disjunction = do
  a <- variable
  _ <- char '|'
  Binary Or a <$> formula

formula :: ReadP Expr
formula = variable +++ conjunction

-- $> readP_to_S formula "p&q"
