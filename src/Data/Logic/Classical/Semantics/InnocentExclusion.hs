{-# LANGUAGE OverloadedLists #-}

module Data.Logic.Classical.Semantics.InnocentExclusion where

import Data.Logic.Classical.Semantics ( isContradiction )
import Data.Logic.Classical.Syntax
    ( toExpr, BOp(Or, And), CExpr, Expr(Binary, Unary), UOp(Not), Var )
import Data.Set as S
    ( Set,
      filter,
      foldr,
      fromList,
      isProperSubsetOf,
      member,
      powerSet,
      toList,
      unions )

-- Checks whether or not a set of propositions is mutually consistent.
isConsistent :: Set CExpr -> Bool
isConsistent exprs = not . isContradiction $ conjoin exprs

-- I really should change this to constants
tautExpr :: CExpr
tautExpr = Binary Or (toExpr 'a') (Unary Not (toExpr 'a'))

conjoin :: Set CExpr -> CExpr
conjoin = S.foldr (Binary And) tautExpr

-- a function from a proposition and a set of alternatives to the compatible subsets
compatible :: CExpr -> Set CExpr -> Set (Set CExpr)
compatible prej altSet =
    S.filter isConsistent
        $   S.fromList
        $   S.fromList
        <$> [ prej : (Unary Not <$> S.toList alts')
            | alts' <- S.toList $ powerSet altSet
            ]

testOr :: Expr Var
testOr = Binary Or (toExpr 'a') (toExpr 'b')

maxSets :: Ord a => Set (Set a) -> Set (Set a)
maxSets compAlts = S.fromList $ concat
    [ [ altSet | not $ any (\altSet' -> altSet `isProperSubsetOf` altSet') compAlts ]
    | altSet <- S.toList compAlts
    ]

ieAlts :: CExpr -> Set CExpr -> Set CExpr
ieAlts prej altset = let
  maxCompSets = maxSets $ compatible prej altset
  in
  unions [ S.fromList [ie | ie <- S.toList maxCompSet, all (\p -> ie `S.member` p) maxCompSets ] | maxCompSet <- S.toList maxCompSets ]

exhIE :: CExpr -> Set CExpr -> CExpr
exhIE p altSet = conjoin (ieAlts p altSet)
