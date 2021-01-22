module Data.Logic.Classical.Alternatives where

import Data.Logic.Classical.Syntax
    ( toExpr, BOp(..), Expr(..), UOp(..), Var )

alts :: Expr a -> [Expr a]
alts (Simple a) = [Simple a]
alts (Unary op a) = [ Unary op' b | b <- alts a, op' <- altUOps op]
alts (Binary And a b) = [ Binary Or alt_a alt_b | alt_a <- a:alts a, alt_b <- b:alts b]
alts (Binary Or a b) = [ Binary And alt_a alt_b | alt_a <- a:alts a, alt_b <- b:alts b]

-- Gives back closure under substitution of binary operators
alts2 :: Expr a -> [Expr a]
alts2 (Simple a  ) = [Simple a]
alts2 (Unary op a) = [ Unary op' b | b <- alts a, op' <- altUOps op]
alts2 (Binary op a b) = [ Binary op' alt_a alt_b | alt_a <- alts a, alt_b <- alts b, op' <- altBOps op]

altBOps :: BOp -> [BOp]
altBOps _ = [And,Or]

altUOps :: UOp -> [UOp]
altUOps Not = [Not]
altUOps Exh = [Exh]

testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- >>> alts testExpr
-- [(p ∧ (q ∨ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∨ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r))]

-- >>> alts2 testExpr
-- [(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∧ r)),(p ∨ (q ∧ r))]
