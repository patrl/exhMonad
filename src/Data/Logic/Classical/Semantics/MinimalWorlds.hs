module Data.Logic.Classical.Semantics.MinimalWorlds where

import Data.Set

import Data.List ( sortBy )

import Data.Logic.Classical.Alternatives

import Data.Logic.Classical.Semantics hiding (testExpr)
import Data.Logic.Classical.Syntax

-- preorder on assignments (i.e., possible worlds) from Spector's paper.
altLeq :: [CExpr] -> Assignment -> Assignment -> Ordering
altLeq altset g h
    | gVerifyingAlts `isProperSubsetOf` hVerifyingAlts = LT
    | gVerifyingAlts == hVerifyingAlts = EQ
    | otherwise = GT
    where
      gVerifyingAlts = (fromList . concat) [[p | evaluate g p == Just True] | p <- altset]
      hVerifyingAlts = (fromList . concat) [[q | evaluate h q == Just True] | q <- altset]


-- Strict pre-order on assignments.
altLT :: [CExpr] -> Assignment -> Assignment -> Bool
altLT alts g g' = (fromList . concat) [[p | evaluate g p == Just True] | p <- alts] `isProperSubsetOf` (fromList . concat) [[q | evaluate g' q == Just True] | q <- alts]

-- exhMW is a modal operator
-- this currently doesn't work
exhMW :: Assignment -> CExpr -> [CExpr] -> Maybe Bool
exhMW g prej altSet = do
  t <- evaluate g prej
  return $ t && none (\h -> evaluate g prej == Just True && altLeq altSet h g == LT) (assignments prej)

none :: Foldable t => (a -> Bool) -> t a -> Bool
none p = not . any p


-- >>> sortBy (altLeq (alts testExpr)) (toList $ assignments testExpr)
-- [fromList [(p,False),(q,False),(r,False)],fromList [(p,False),(q,False),(r,True)],fromList [(p,False),(q,True),(r,False)],fromList [(p,False),(q,True),(r,True)],fromList [(p,True),(q,False),(r,False)],fromList [(p,True),(q,False),(r,True)],fromList [(p,True),(q,True),(r,False)],fromList [(p,True),(q,True),(r,True)]]

-- >>> simpleEx = Binary Or (toExpr 'p') (toExpr 'q')
-- >>> simpleEx
-- (p ∨ q)
-- >>> altsS simpleEx
-- [p,q,(p ∧ q),(p ∨ q)]
-- >>> [(g,exhMW g simpleEx (altsS simpleEx)) | g <- toList $ assignments simpleEx]
-- [(fromList [(p,False),(q,False)],Just False),(fromList [(p,False),(q,True)],Just False),(fromList [(p,True),(q,False)],Just False),(fromList [(p,True),(q,True)],Just False)]
