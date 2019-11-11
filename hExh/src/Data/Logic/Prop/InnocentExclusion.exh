module Data.Logic.Prop.InnocentExclusion where

import Control.Monad (filterM)

subsets :: [a] -> [[a]]
subsets = filterM (const [True, False])

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset a b = all (`elem` b) a

isMaximum :: Eq a => [a] -> [[a]] -> Bool
isMaximum x xs = not $ any (\x' -> isSubset x x' && x /= x') xs && x `elem` xs

removeNonMaximal :: Eq a => [[a]] -> [[a]]
removeNonMaximal xs = filter (`isMaximum` xs) xs
