module Data.Logic.Classical where

import Data.List (nub)
-- This datatype characterizes the set of wffs of classical logic
-- I'm factoring out binary and unary operators to make this more readily extensible.
data Expr = Var String | Unary UOp Expr | Binary BOp Expr Expr

data UOp = Not

uAlts :: [UOp]
uAlts = [Not]

data BOp = And | Or

bAlts :: [BOp]
bAlts = [And, Or]

instance Show Expr where
  show (Var v) = v
  show (Unary op expr) = show op ++ " " ++ "(" ++ show expr ++ ")"
  show (Binary op expr1 expr2) = "(" ++ unwords [show expr1, show op, show expr2] ++ ")"

instance Show BOp where
  show And = "&"
  show Or = "|"

instance Show UOp where
  show Not = "~"

---

-- list all of the distinct variables, removing duplicates.
variables :: Expr -> [String]
variables (Unary _ expr) = variables expr
variables (Binary _ expr1 expr2) = nub $ variables expr1 ++ variables expr2
variables (Var v) = [v]
