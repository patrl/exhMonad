{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}

module Data.Logic.Final where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad                  ( replicateM )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

-----------------------------------------------------
-- A tagless final encoding of boolean expressions --
-----------------------------------------------------

-- The tagless final encoding of boolean expressions.
class BoolSYM s where
  {-# MINIMAL top, neg, (conj|disj|impl) #-}
  top :: s
  bot :: s
  bot = neg top
  neg :: s -> s
  conj :: s -> s -> s
  conj s t = neg (neg s `disj` neg t)
  disj :: s -> s -> s
  disj s = impl (neg s)
  impl :: s -> s -> s
  impl s t = neg (neg s `disj` neg t)

-- Newtype wrappers to distinguish between ascii and unicode strings.
newtype A = A String
newtype U = U String

-- Printing boolean formulas as ascii strings.
instance BoolSYM A where
  top = A "T"
  bot = A "F"
  neg (A s) = A $ "~" ++ s
  conj (A s) (A t) = A $ "(" ++ s ++ "&" ++ t ++ ")"
  disj (A s) (A t) = A $ "(" ++ s ++ "|" ++ t ++ ")"
  impl (A s) (A t) = A $ "(" ++ s ++ "->" ++ t ++ ")"

viewA :: A -> String
viewA (A s) = s

-- Printing boolean formulas as unicode strings
instance BoolSYM U where
  top = U "⊤"
  bot = U "⊥"
  neg (U s) = U $ "¬" ++ s
  conj (U s) (U t) = U $ "(" ++ s ++ "∧" ++ t ++ ")"
  disj (U s) (U t) = U $ "(" ++ s ++ "∨" ++ t ++ ")"
  impl (U s) (U t) = U $ "(" ++ s ++ "→" ++ t ++ ")"

viewU :: U -> String
viewU (U s) = s

-- Evaluating boolean formulas.

instance BoolSYM Bool where
  top  = True
  bot  = False
  neg  = not
  conj = (&&)
  disj = (||)

eval :: Bool -> Bool
eval = id

-- Evaluation, but we keep track of intermediate results.

instance BoolSYM a => BoolSYM [(a,Bool)] where
  top = [(top, top)]
  bot = [(bot, bot)]
  neg ps = [ (neg f, neg g) | (f, g) <- ps ] ++ [ (f, g) | (f, g) <- ps ]
  conj ps qs =
    [ (conj a b, conj p q) | (a, p) <- ps, (b, q) <- qs ]
      ++ [ (a, b) | (a, b) <- ps ]
      ++ [ (c, d) | (c, d) <- qs ]

stepEval :: [(A, Bool)] -> [(String, Bool)]
stepEval ps = [ (s, t) | (A s, t) <- ps ]

----------------------------------------------
-- Modular extension to propositional logic --
----------------------------------------------

-- We treat variables as integers.
type Var = Int

-- An assignment is a function from variables to boolean values.
type Assignment = M.Map Var Bool

-- 'at' maps variables to expressions of propositional logic, which compose via the boolean operators.
-- N.b., we don't need to define the boolean operators again for propositional logic.
class BoolSYM s => PropSYM s where
  at :: Var -> s

-- Printing variables as ascii/unicode.
instance PropSYM A where
  at = A . show

instance PropSYM U where
  at = U . show

instance BoolSYM (S.Set Var) where
  top  = S.empty
  neg  = id
  conj = S.union
  disj = S.union

instance PropSYM (S.Set Var) where
  at n = S.singleton n

variables :: S.Set Var -> [Var]
variables = S.toList

instance BoolSYM [Assignment] where
  top  = mempty
  neg  = id
  conj = (++)
  disj = (++)

instance PropSYM [Assignment] where
  at n =
    let vars = variables (at n)
    in  [ M.fromList [ (v, t) | v <- vars | t <- ts ]
        | ts <- replicateM (length vars) [True, False]
        ]

universe :: [Assignment] -> [Assignment]
universe = id

instance BoolSYM (Assignment -> Maybe Bool) where
  top = const $ pure top
  neg p = (fmap . fmap) neg p
  conj p q = (liftA2 . liftA2) conj p q
  disj p q = (liftA2 . liftA2) disj p q

instance PropSYM (Assignment -> Maybe Bool) where
  at n g = M.lookup n g

evalProp :: (Assignment -> Maybe Bool) -> Assignment -> Maybe Bool
evalProp = id

tf1 = at 1 `conj` at 2 `conj` at 1

-- >>> universe tf1
-- [fromList [(1,True)],fromList [(1,False)],fromList [(2,True)],fromList [(2,False)],fromList [(1,True)],fromList [(1,False)]]
