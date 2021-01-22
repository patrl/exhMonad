module Data.Logic.Classical.Alternatives where

import           Data.Logic.Classical.Syntax    ( BOp(..)
                                                , Expr(..)
                                                , UOp(..)
                                                , Var
                                                , toExpr
                                                )

import Data.Logic.Classical.Parser (parseExpr)


isSimple :: Expr a -> Bool
isSimple (Simple _) = True
isSimple _ = False

-- A more restrictive algorithm for computing alternatives
-- Note this only gets exclusivity with three disjuncts (with embedded exh), it breaks down with four
alts :: Expr a -> [Expr a]
alts (Simple a  ) = [Simple a]
alts (Unary op a) = [ Unary op' b | b <- alts a, op' <- altUOps op ]
alts (Binary op a b) = let
  altsA = if isSimple a then alts a else a:alts a
  altsB = if isSimple b then alts b else b:alts b
  altOp = if op == And then Or else And
  in
    [ Binary altOp alt_a alt_b | alt_a <- altsA, alt_b <- altsB ]

-- Gives back closure under substitution of binary operators
alts2 :: Expr a -> [Expr a]
alts2 (Simple a  ) = [Simple a]
alts2 (Unary op a) = [ Unary op' b | b <- alts2 a, op' <- altUOps op ]
alts2 (Binary op a b) =
    [ Binary op' alt_a alt_b
    | alt_a <- alts2 a
    , alt_b <- alts2 b
    , op'   <- altBOps op
    ]

-- efficiently computes Sauerland alternatives; closure of a formula under sub-formulas and substitution of binary operators
altsS :: Expr a -> [Expr a]
altsS expr
    | (Simple a) <- expr                       = [Simple a]
    | (Unary op a) <- expr = [ Unary op' b | b <- altsS a, op' <- altUOps op ]
    | (Binary op (Simple a) (Simple b)) <- expr = Simple a:Simple b:[Binary op' (Simple a) (Simple b) | op' <- altBOps op]
    | (Binary op (Simple a) b) <- expr          = Simple a : b : altsS b ++ [Binary op' (Simple a) alt_b | alt_b <- altsS b, op' <- altBOps op]
    | (Binary op a (Simple b)) <- expr          = a : Simple b : altsS a ++ [Binary op' alt_a (Simple b) | alt_a <- altsS a, op' <- altBOps op]
    | (Binary op a b) <- expr                   = a : b : altsS a ++ altsS b ++ [ Binary op' alt_a alt_b | alt_a <- alts2 a, alt_b <- alts2 b, op'   <- altBOps op]

altBOps :: BOp -> [BOp]
altBOps _ = [And, Or]

altUOps :: UOp -> [UOp]
altUOps Not = [Not]
altUOps Exh = [Exh]

testExpr :: Expr Var
testExpr = Binary Or (toExpr 'p') (Binary Or (toExpr 'q') (toExpr 'r'))

-- >>> testExpr
-- (p ∨ (q ∨ r))

-- >>> alts testExpr
-- [(p ∧ (q ∨ r)),(p ∧ (q ∧ r))]
--

-- >>> alts2 testExpr
-- [(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∨ r)),(p ∨ (q ∨ r))]

-- >>> altsS testExpr
-- [p,(q ∨ r),q,r,(q ∧ r),(q ∨ r),(p ∧ q),(p ∨ q),(p ∧ r),(p ∨ r),(p ∧ (q ∧ r)),(p ∨ (q ∧ r)),(p ∧ (q ∨ r)),(p ∨ (q ∨ r))]
