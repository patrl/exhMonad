module Data.Logic.Classical where

import Text.ParserCombinators.ReadP

newtype Var = Var Char deriving (Eq)

instance Show Var where
  show (Var c) = [c]

-- This datatype characterizes the set of wffs of classical logic
-- I'm factoring out binary and unary operators to make this more readily extensible.
data Expr = Variable Var | Unary UOp Expr | Binary BOp Expr Expr

data UOp = Not

uAlts :: [UOp]
uAlts = [Not]

data BOp = And | Or

bAlts :: [BOp]
bAlts = [And, Or]

instance Show Expr where
  show (Variable v) = show v
  show (Unary op expr) = show op ++ " " ++ "(" ++ show expr ++ ")"
  show (Binary op expr1 expr2) = "(" ++ unwords [show expr1, show op, show expr2] ++ ")"

instance Show BOp where
  show And = "&"
  show Or = "|"

instance Show UOp where
  show Not = "~"

formalAlts :: Expr -> [Expr]
formalAlts (Unary _ expr) = [Unary alt_op alt_expr | alt_op <- uAlts, alt_expr <- formalAlts expr]
formalAlts (Binary _ expr1 expr2) = [Binary alt_op alt_expr1 alt_expr2 | alt_op <- bAlts, alt_expr1 <- formalAlts expr1, alt_expr2 <- formalAlts expr2]
formalAlts (Variable v) = [Variable v]

-- $> formalAlts $ Binary Or (Variable $ Var 'a') (Binary Or (Variable $ Var 'b') (Variable $ Var 'c'))

------------
-- parser --
------------

binOps :: [(BOp, Char)]
binOps = [(And, '&'), (Or, '|')]

uOps :: [(UOp, Char)]
uOps = [(Not, '~')]

-- parser for simple variables
variable :: ReadP Expr
variable = do
  s <- choice (map char $ ['a' .. 'z'] ++ ['A' .. 'Z'])
  return $ Variable $ Var s

-- parser for parentheses
brackets :: ReadP a -> ReadP a
brackets p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

formula2 :: ReadP Expr
formula2 =
  foldr
    ( \(op, name) p ->
        let this =
              p +++ do
                a <- p +++ brackets formula2
                _ <- char name
                b <- this
                return (Binary op a b)
         in this
    )
    (variable +++ brackets formula2)
    binOps

-- $> readP_to_S formula2 "p|q&r"

--------------------------
-- Normalization rules --
--------------------------

doubleNegationElim :: Expr -> Maybe Expr
doubleNegationElim (Unary Not (Unary Not expr)) = Just expr
doubleNegationElim _ = Nothing

deMorgan1 :: Expr -> Maybe Expr
deMorgan1 (Unary Not (Binary Or expr1 expr2)) = Just (Binary And (Unary Not expr1) (Unary Not expr2))
deMorgan1 _ = Nothing
