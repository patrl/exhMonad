{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Data.Logic.Prop.Parser where

import Data.Logic.Prop.Formula (Expr (..), Var (..))

import Text.ParserCombinators.Parsec
    ((<|>), char, choice, eof, letter, parse, spaces, string, try)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Pos (SourceName)
import Text.ParserCombinators.Parsec.Prim (GenParser)

parseExpr :: SourceName -> String -> Either ParseError Expr
parseExpr = parse statement

statement :: GenParser Char st Expr
statement = do spaces
               x <- try binary <|> expr
               spaces
               eof
               return x

expr :: GenParser Char st Expr
expr = choice [binaryP, negation, variable]

variable :: GenParser Char st Expr
variable = Variable . Var <$> letter

negation :: GenParser Char st Expr
negation = do char '~'
              spaces
              Negation <$> expr

binaryP :: GenParser Char st Expr
binaryP = do char '('
             spaces
             x <- binary
             spaces
             char ')'
             return x

binary :: GenParser Char st Expr
binary = do x1 <- expr
            spaces
            s  <- choice $ map string ["&", "|"]
            spaces
            connective s x1 <$> expr
  where
    connective c = case c of
      "&"   -> Conjunction
      "|"   -> Disjunction
      _     -> error "Impossible case"
