module Data.Logic.Classical.Semantics.MinimalWorlds where

import Data.Set

import Data.List ( sortBy )

import Data.Logic.Classical.Alternatives

import Data.Logic.Classical.Semantics hiding (testExpr)
import Data.Logic.Classical.Syntax

-- preorder on assignments (i.e., possible worlds) from Spector's paper.
altLeq :: [CExpr] -> Assignment -> Assignment -> Ordering
altLeq alts g h
    | gVerifyingAlts `isProperSubsetOf` hVerifyingAlts = LT
    | gVerifyingAlts == hVerifyingAlts = EQ
    | otherwise = GT
    where
      gVerifyingAlts = (fromList . concat) [[p | evaluate g p == Just True] | p <- alts]
      hVerifyingAlts = (fromList . concat) [[q | evaluate h q == Just True] | q <- alts]


-- Strict pre-order on assignments.
altLT :: [CExpr] -> Assignment -> Assignment -> Bool
altLT alts g g' = (fromList . concat) [[p | evaluate g p == Just True] | p <- alts] `isProperSubsetOf` (fromList . concat) [[q | evaluate g' q == Just True] | q <- alts]


-- >>> sortBy (altLeq (alts testExpr)) (toList $ assignments testExpr)
-- [fromList [(p,False),(q,False),(r,False)],fromList [(p,False),(q,False),(r,True)],fromList [(p,False),(q,True),(r,False)],fromList [(p,False),(q,True),(r,True)],fromList [(p,True),(q,False),(r,False)],fromList [(p,True),(q,False),(r,True)],fromList [(p,True),(q,True),(r,False)],fromList [(p,True),(q,True),(r,True)]]
