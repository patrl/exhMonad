module Data.Logic.ExhProp.Formula where

newtype Var = Var Char
    deriving (Eq, Ord)

data Expr = Variable Var
          | Negation Expr
          | Exh Expr
          | ExhIE Expr
          | Conjunction Expr Expr
          | Disjunction Expr Expr
          deriving Eq

instance Show Var where
    show (Var v) = [v]
