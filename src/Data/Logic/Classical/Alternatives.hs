module Data.Logic.Classical.Alternatives where

import Data.Logic.Classical.Syntax

-- this algorithm swaps out binary connectives
alts :: CExpr -> [CExpr]
alts (Simple a) = [Simple a]
alts (Unary op a) = [Unary op b | b <- alts a]
alts (Binary Or a b) = [Binary And alt_a alt_b | alt_a <- alts a, alt_b <- alts b]
alts (Binary And a b) = [Binary Or alt_a alt_b | alt_a <- alts a, alt_b <- alts b]


testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- >>> alts testExpr
-- [(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∨ r)),(p ∨ (q ∨ r))]
