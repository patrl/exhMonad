module Data.Logic.Classical.Alternatives where

import Data.Logic.Classical.Syntax
    ( toExpr, BOp(..), Expr(..), UOp(..), Var )

-- this algorithm swaps out binary connectives
alts :: Expr a -> [Expr a]
alts (Simple a  ) = [Simple a]
alts (Unary op a) = [ Unary op' b | b <- alts a, op' <- altUOps op]
alts (Binary op a b) = [ Binary op' alt_a alt_b | alt_a <- alts a, alt_b <- alts b, op' <- altBOps op]

altBOps :: BOp -> [BOp]
altBOps _ = [And,Or]

altUOps :: UOp -> [UOp]
altUOps Not = [Not]
altUOps Exh = [Exh]

testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- >>> alts testExpr
-- [(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∨ r)),(p ∨ (q ∨ r))]

-- >>> altsExcl testExpr
-- [(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∨ r))]
