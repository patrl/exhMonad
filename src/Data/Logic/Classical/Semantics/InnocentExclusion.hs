module Data.Logic.Classical.Semantics.InnocentExclusion where

import Data.Logic.Classical.Alternatives

import Data.Logic.Classical.Semantics hiding (testExpr)
import Data.Logic.Classical.Syntax
import Data.Logic.Classical.Parser

-- Checks whether or not a set of propositions is mutually consistent.
isConsistent :: [CExpr] -> Bool
isConsistent exprs = not . isContradiction $ conjoin exprs

-- >>> isConsistent <$> (sequence [parseExpr "~a", parseExpr "a&b"])
-- Right False

-- I really should change this to constants
tautExpr :: CExpr
tautExpr = Binary Or (toExpr 'a') (Unary Not (toExpr 'a'))

conjoin :: [CExpr] -> CExpr
conjoin = foldr (Binary And) tautExpr

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

-- a function from a proposition and a set of alternatives to the compatible subsets
compatible :: CExpr -> [CExpr] -> [[CExpr]]
compatible prej altSet = filter isConsistent [prej:(Unary Not <$> alts') | alts' <- powerset altSet]

testOr :: Expr Var
testOr = Binary Or (toExpr 'a') (toExpr 'b')

maxAlts :: [[CExpr]] -> [[CExpr]]
maxAlts = undefined

-- >>> compatible testOr (altsS testOr)
-- [[(a ∨ b),¬ (a),¬ ((a ∧ b))],[(a ∨ b),¬ (a)],[(a ∨ b),¬ (b),¬ ((a ∧ b))],[(a ∨ b),¬ (b)],[(a ∨ b),¬ ((a ∧ b))],[(a ∨ b)]]
