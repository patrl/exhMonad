{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Logic.Final where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad                  ( replicateM )
import           Data.Biapplicative             ( (<<*>>)
                                                , biliftA2
                                                , bipure
                                                )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

-----------------------------------------------------
-- A tagless final encoding of boolean expressions --
-----------------------------------------------------

-- The tagless final encoding of boolean expressions.
class BoolSYM s where
  {-# MINIMAL top, neg, (conj|disj) #-}
  top :: s
  bot :: s
  bot = neg top
  neg :: s -> s
  conj :: s -> s -> s
  conj s t = neg (neg s `disj` neg t)
  disj :: s -> s -> s
  disj s t = neg (neg s `conj` neg t)

-- These look like bifunctors
instance (BoolSYM a,BoolSYM b) => BoolSYM (a,b) where
  top = (top, top)
  bot = (bot, bot)
  neg p = (neg, neg) <<*>> p
  conj p q = biliftA2 conj conj p q
  disj p q = biliftA2 disj disj p q

instance (BoolSYM a,BoolSYM b,BoolSYM c) => BoolSYM (a,b,c) where
  top = (top, top, top)
  bot = (bot, bot, bot)
  neg (s, t, u) = (neg s, neg t, neg u)
  conj (s, t, u) (s', t', u') = (conj s s', conj t t', conj u u')
  disj (s, t, u) (s', t', u') = (disj s s', disj t t', disj u u')

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

viewA :: A -> String
viewA (A s) = s

-- Printing boolean formulas as unicode strings
instance BoolSYM U where
  top = U "⊤"
  bot = U "⊥"
  neg (U s) = U $ "¬" ++ s
  conj (U s) (U t) = U $ "(" ++ s ++ "∧" ++ t ++ ")"
  disj (U s) (U t) = U $ "(" ++ s ++ "∨" ++ t ++ ")"

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

-- -- Evaluation, but we keep track of intermediate results.

-- instance BoolSYM a => BoolSYM [(a,Bool)] where
--   top = [(top, top)]
--   bot = [(bot, bot)]
--   neg ps = [ (neg f, neg g) | (f, g) <- ps ] ++ [ (f, g) | (f, g) <- ps ]
--   conj ps qs =
--     [ (conj a b, conj p q) | (a, p) <- ps, (b, q) <- qs ]
--       ++ [ (a, b) | (a, b) <- ps ]
--       ++ [ (c, d) | (c, d) <- qs ]

-- stepEval :: [(A, Bool)] -> [(String, Bool)]
-- stepEval ps = [ (s, t) | (A s, t) <- ps ]

----------------------------------------------
-- Modular extension to propositional logic --
----------------------------------------------

-- We treat variables as integers.
type Var = Int

-- An assignment is a mapping from variables to boolean values.
type Assignment = M.Map Var Bool

-- 'at' maps variables to expressions of propositional logic, which compose via the boolean operators.
-- N.b., we don't need to define the boolean operators again for propositional logic.
class BoolSYM s => PropSYM s where
  at :: Var -> s

-- Some boilerplate duplicator instances.
instance (PropSYM a, PropSYM b) => PropSYM (a,b) where
  at n = (at n, at n)

instance (PropSYM a, PropSYM b,PropSYM c) => PropSYM (a,b,c) where
  at n = (at n, at n, at n)

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

-- We could compute this compositionally, but there isn't much point.
universe :: S.Set Var -> [Assignment]
universe vs =
  [ M.fromList [ (v, t) | v <- variables vs | t <- ts ]
  | ts <- replicateM (length vs) [True, False]
  ]

-- We want to use a function from g to maybe a, since Map doesn't have an applicaative instance.
type Proposition = Assignment -> Maybe Bool

-- Note that we're just composing applicative functors here.
instance BoolSYM Proposition where
  top = (pure . pure) top
  neg p = (fmap . fmap) neg p
  conj p q = (liftA2 . liftA2) conj p q
  disj p q = (liftA2 . liftA2) disj p q

instance PropSYM Proposition where
  at n g = M.lookup n g

evalProp :: Proposition -> Proposition
evalProp = id

verifiers :: (S.Set Var, Proposition) -> S.Set Assignment
verifiers (vs, p) = S.fromList [ g | g <- universe vs, p g == Just True ]

falsifiers :: (S.Set Var, Proposition) -> S.Set Assignment
falsifiers (vs, p) = S.fromList [ g | g <- universe vs, p g == Just False ]

entails :: (S.Set Var, Proposition) -> (S.Set Var, Proposition) -> Bool
entails (vs, p) (us, q) =
  let vs' = vs `S.union` us
  in  verifiers (vs', p) `S.isSubsetOf` verifiers (vs', q)

equiv :: (S.Set Var, Proposition) -> (S.Set Var, Proposition) -> Bool
equiv (vs, p) (us, q) =
  let vs' = vs `S.union` us in verifiers (vs', p) == verifiers (vs', q)

data PropType = Tautology | Contradiction | Contingency deriving (Eq,Show)

propType :: (S.Set Var, Proposition) -> PropType
propType (vs, p) | universe vs == S.toList (verifiers (vs, p)) = Tautology
                 | universe vs == S.toList (falsifiers (vs, p)) = Contradiction
                 | otherwise = Contingency

isTautology :: (S.Set Var, Proposition) -> Bool
isTautology (vs, p) = propType (vs, p) == Tautology

isContradiction :: (S.Set Var, Proposition) -> Bool
isContradiction (vs, p) = propType (vs, p) == Contradiction

isContingency :: (S.Set Var, Proposition) -> Bool
isContingency (vs, p) = propType (vs, p) == Contingency


--------------------------------------------
-- indeterminacy and formal alternatives --
--------------------------------------------

newtype AltScalar s = AltScalar [s] deriving (Functor,Applicative,Semigroup,Monoid)

instance BoolSYM s => BoolSYM (AltScalar s) where
  top = pure top
  bot = pure bot
  neg p = neg <$> p
  conj p q = liftA2 disj p q
  disj p q = liftA2 conj p q

instance PropSYM s => PropSYM (AltScalar s) where
  at n = pure $ at n

alts :: AltScalar s -> [s]
alts (AltScalar ss) = ss

viewAlts :: AltScalar A -> [String]
viewAlts (AltScalar as) = viewA <$> as

-- We can blindly negate structural alternatives fairly easily, but this isn't enough.
exhaust :: PropSYM p => (AltScalar p, p) -> p
exhaust (AltScalar as, p) = p `conj` conjoinNegs as

conjoinNegs :: BoolSYM s => [s] -> s
conjoinNegs as = foldr conj top (neg <$> as)

tf1 :: PropSYM s => s
tf1 = at 1 `disj` at 2

----------------------------
-- Exhaustification logic --
----------------------------

class (PropSYM s) => ExhSYM s where
  exh :: s -> s

instance ExhSYM A where
  exh (A p) = A $ "O" ++ p

instance ExhSYM U where
  exh (U p) = U $ "𝒪" ++ p

-- Boilperplate duplicator instances.

instance (ExhSYM a, ExhSYM b) => ExhSYM (a,b) where
  exh p = bipure exh exh <<*>> p


-- Scalar alternatives foe exhaustified formulas.
instance ExhSYM s => ExhSYM (AltScalar s) where
  exh p = exh <$> p

--- Not sure this quite works yet.
excludable
  :: (S.Set Var, Proposition, AltScalar (Proposition, A)) -> [(Proposition, A)]
excludable (vs, p, AltScalar as) =
  [ (a, s) | (a, s) <- as, (not . isContradiction) (vs, p `conj` a) ]

instance ExhSYM (S.Set Var, Proposition, AltScalar (Proposition,A)) where
  exh (vs, p, as) =
    ( vs
    , p `conj` foldr conj top (neg <$> [ a | (a, _) <- excludable (vs, p, as) ])
    , as
    )

tf2 :: ExhSYM s => s
tf2 = exh tf1

tf3 :: ExhSYM s => s
tf3 = tf2 `conj` at 3

verifiersExh
  :: (S.Set Var, Proposition, AltScalar (Proposition, A)) -> S.Set Assignment
verifiersExh (vs, p, _) = verifiers (vs, p)

-- >>> viewAlts tf2
-- ["O(1&2)"]

-- >>> excludable tf3
-- No instance for (Show (Assignment -> Maybe Bool))
--
--   (maybe you haven't applied a function to enough arguments?)
--
--   (maybe you haven't applied a function to enough arguments?)

-- >>> viewExhaust tf3
-- "((3|((1|2)&(~(1&2)&T)))&(~(3&O(1&2))&T))"

-- >>> viewExhAlts tf2
-- ["O(1&2)"]
