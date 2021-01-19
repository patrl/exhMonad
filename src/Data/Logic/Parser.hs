module Data.Logic.Parser where

-- a parser for expressions of classical logic.

import Control.Monad (liftM)
import Data.Logic.Classical
  ( BOp (And, Or),
    Expr (..),
    UOp (Not),
  )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok


languageDef =
  emptyDef
    { Tok.identStart = letter,
      Tok.identLetter = alphaNum,
      Tok.reservedOpNames = ["&", "|", "~"]
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser Expr -> Parser Expr
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parseExpr :: Parser Expr
parseExpr = buildExpressionParser operators parseVariable

parseVariable :: Parser Expr
parseVariable =
  parens parseExpr
    <|> fmap Var identifier

operators :: [[Operator Char () Expr]]
operators =
  [ [ Prefix (reservedOp "~" >> return (Unary Not)),
      Infix (reservedOp "&" >> return (Binary And)) AssocRight,
      Infix (reservedOp "|" >> return (Binary Or)) AssocRight
    ]
  ]

parseInput :: Parser Expr
parseInput = do
  whiteSpace
  ex <- parseExpr
  eof
  return ex

-- >>> parse parseInput "" "~ p | q & r"
-- Right (~ (p) | (q & r))
