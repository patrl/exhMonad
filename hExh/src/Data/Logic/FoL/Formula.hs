module Data.Logic.FoL.Formula where

newtype Var = Var Char
    deriving (Eq, Ord)

instance Show Var where
  show (Var v) = show v

data Term = Variable Var | Struct String [Term] deriving (Eq,Ord)

instance Show Term where
  show (Variable v) = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts
