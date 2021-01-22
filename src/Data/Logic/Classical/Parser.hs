module Data.Logic.Classical.Parser where

import           Data.Functor.Identity          ( Identity )
import Data.Logic.Classical.Syntax
    ( BOp(Or, And), CExpr, Expr(..), UOp(Exh, Not), Var(..) )
import           Text.Parsec.Language           ( emptyDef )
import Text.ParserCombinators.Parsec
    ( alphaNum, (<|>), parse, ParseError, Parser )
import Text.ParserCombinators.Parsec.Expr
    ( Assoc(AssocRight),
      buildExpressionParser,
      Operator(Prefix, Infix) )
import qualified Text.ParserCombinators.Parsec.Token
                                               as P

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef
parens :: Parser CExpr -> Parser CExpr
parens = P.parens lexer
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parseSimple :: Parser CExpr
parseSimple = parens expr <|> Simple . Var <$> alphaNum

table :: [[Operator Char () CExpr]]
table =
    [ [unary "~" (Unary Not), unary "O" (Unary Exh)]
    , [binary "&" (Binary And) AssocRight, binary "|" (Binary Or) AssocRight]
    ]

binary :: String -> (CExpr -> CExpr -> CExpr) -> Assoc -> Operator Char () CExpr
binary name fun = Infix $ do
    reservedOp name
    return fun

unary :: String -> (CExpr -> CExpr) -> Operator Char () CExpr
unary name fun = Prefix $ do
        reservedOp name
        return fun

expr :: Parser CExpr
expr = buildExpressionParser table parseSimple

parseExpr :: [Char] -> Either ParseError (Expr Var)
parseExpr = parse expr ""

-- >>> parseExpr "O(a|b)"
-- Right 𝒪 ((a ∨ b))
