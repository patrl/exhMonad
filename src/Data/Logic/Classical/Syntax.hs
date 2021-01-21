{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Logic.Classical.Syntax where

-- An Abstract Syntax Tree for logical expressions
data Expr a = Simple a | Unary UOp (Expr a) | Binary BOp (Expr a) (Expr a) deriving (Eq, Foldable, Traversable, Functor)

type CExpr = Expr Var

type BExpr = Expr Bool

newtype Var = Var Char deriving (Eq, Ord)

toExpr :: Char -> CExpr
toExpr c = Simple $ Var c

instance Show Var where
  show (Var c) = [c]

data UOp = Not deriving (Eq)

interpretU :: UOp -> (Bool -> Bool)
interpretU Not = not

data BOp = And | Or deriving (Eq)

interpretB :: BOp -> (Bool -> Bool -> Bool)
interpretB And = (&&)
interpretB Or = (||)

instance Show CExpr where
  show (Simple v) = show v
  show (Unary op expr) = show op ++ " " ++ "(" ++ show expr ++ ")"
  show (Binary op expr1 expr2) = "(" ++ unwords [show expr1, show op, show expr2] ++ ")"

instance Show BOp where
  show And = "∧"
  show Or = "∨"

instance Show UOp where
  show Not = "¬"
