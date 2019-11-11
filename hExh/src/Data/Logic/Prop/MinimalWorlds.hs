module Data.Logic.Prop.MinimalWorlds (exhMw, valuesExhMw) where

import Data.Logic.Prop.Formula
import Data.Logic.Prop.Semantics
import Control.Monad.Pointed

strictPreorder :: [Expr] -> Assignment -> Assignment -> Ordering
strictPreorder alts u v
    | uAlts `isPropersubset` vAlts = LT
    | vAlts `isPropersubset` uAlts = GT
    | otherwise = EQ
        where uAlts = [p | p <- alts, interpret p u]
              vAlts = [q | q <- alts, interpret q v]

exhMw :: Pointed Expr -> Assignment -> Bool
exhMw (Pointed p alts) w = interpret p w && null [v | v <- assignments p, interpret p v && strictPreorder alts v w == LT]

valuesExhMw :: Pointed Expr -> [Bool]
valuesExhMw (Pointed p alts) = map (exhMw (Pointed p alts)) (assignments p)

isPropersubset :: Eq a => [a] -> [a] -> Bool
isPropersubset x y = all (`elem` y) x && x /= y
