{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Logic.Final where

import Control.Applicative ( Applicative(liftA2) )

type Atom = Int

class PropSYM s where
  top :: s
  bot :: s
  at :: Atom -> s
  neg :: s -> s
  conj :: s -> s -> s
  disj :: s -> s -> s

instance PropSYM String where
  top = "T"
  bot = "F"
  at = show
  neg = (++) "~"
  conj t u = "(" ++ t ++ "|" ++ u ++ ")"
  disj t u = "(" ++ t ++ "&" ++ u ++ ")"

view :: String -> String
view = id

tf1 :: PropSYM s => s
tf1 = at 1 `conj` top

instance PropSYM [Atom] where
  top = []
  bot = []
  at = pure
  neg = id
  conj = (++)
  disj = (++)

variables :: [Atom] -> [Atom]
variables = id

-- >>> variables tf1

type Assignment = Int -> Bool

instance PropSYM (Assignment -> Bool) where
  top = pure True
  bot = pure False
  at n g = g n
  neg = fmap not
  conj = liftA2 (&&)
  disj = liftA2 (||)

eval :: (Assignment -> Bool) -> Assignment -> Bool
eval = id

g1 :: Assignment
g1 1 = True
g1 _ = False



-- >>> view tf1
-- "(1&T)"

-- >>> eval tf1 g1
-- True
