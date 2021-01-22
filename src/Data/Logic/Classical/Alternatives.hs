module Data.Logic.Classical.Alternatives where

import Data.Logic.Classical.Syntax
    ( toExpr, BOp(..), Expr(..), UOp(..), Var )

-- this algorithm swaps out binary connectives
alts :: Expr a -> [Expr a]
alts (Simple a  ) = [Simple a]
alts (Unary op a) = [ Unary op' b | b <- alts a, op' <- altUOps op]
alts (Binary And a b) = [ Binary Or alt_a alt_b | alt_a <- a:alts a, alt_b <- b:alts b]
alts (Binary Or a b) = [ Binary And alt_a alt_b | alt_a <- a:alts a, alt_b <- b:alts b]

altBOps :: BOp -> [BOp]
altBOps _ = [And,Or]

altUOps :: UOp -> [UOp]
altUOps Not = [Not]
altUOps Exh = [Exh]

testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- >>> alts testExpr
-- [(p ∧ (q ∨ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∨ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r)),(p ∧ (q ∧ r))]
