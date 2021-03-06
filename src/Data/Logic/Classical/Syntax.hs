{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Logic.Classical.Syntax where

-- An Abstract Syntax Tree for logical expressions
data Expr a = Simple a | Unary UOp (Expr a) | Binary BOp (Expr a) (Expr a) deriving (Eq, Ord, Foldable, Traversable, Functor)

-- >>> :k Z
-- Z :: Nat

-- >>> :k Unary
-- Unary :: forall a. UOp -> Expr a -> Expr a

type CExpr = Expr Var

type BExpr = Expr Bool

newtype Var = Var Char deriving (Eq, Ord)

toExpr :: Char -> CExpr
toExpr c = Simple $ Var c

instance Show Var where
  show (Var c) = [c]

-- >>> :i UOp
-- unknown command 'i'

data UOp = Not | Exh deriving (Eq,Ord)

uOps :: [UOp]
uOps = [Not,Exh]

data BOp = And | Or deriving (Eq,Ord)

bOps :: [BOp]
bOps = [And,Or]

-- >>> :t And
-- And :: BOp

-- >>> :k UOp
-- UOp :: *

-- >>> :k Unary
-- Unary :: forall a. UOp -> Expr a -> Expr a

instance Show CExpr where
  show (Simple v) = show v
  show (Unary op expr) = show op ++ " " ++ "(" ++ show expr ++ ")"
  show (Binary op expr1 expr2) = "(" ++ unwords [show expr1, show op, show expr2] ++ ")"

instance Show BOp where
  show And = "∧"
  show Or = "∨"

instance Show UOp where
  show Not = "~"
  show Exh = "O"
  -- show Box = "[]"
  -- show Loz = "<>"

-- How to extend this to other classes of logic?
