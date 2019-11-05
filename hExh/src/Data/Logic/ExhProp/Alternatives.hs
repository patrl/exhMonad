module Data.Logic.ExhProp.Alternatives where

import Data.Logic.ExhProp.Formula

binaryConnectives :: [Expr -> Expr -> Expr]
binaryConnectives = [Conjunction,Disjunction]

class Alt a where
    alt :: a -> [a]

scalarAlts :: Expr -> Expr -> [Expr]
scalarAlts expr1 expr2 = [f' expr1' expr2' | f' <- binaryConnectives, expr1' <- alt expr1, expr2' <- alt expr2]

instance Alt Expr where
    alt (Negation expr) = alt expr ++ map Negation (alt expr)
    alt (Conjunction expr1 expr2) = scalarAlts expr1 expr2 ++ alt expr1 ++ alt expr2
    alt (Disjunction expr1 expr2) = scalarAlts expr1 expr2 ++ alt expr1 ++ alt expr2
    alt expr = [expr]

-- >>> alt $ mkVar 'a' `Disjunction` mkVar 'b'
-- [(a && b),(a || b),a,b]

-- >>> alt $ mkVar 'a' `Disjunction` mkVar 'b'
