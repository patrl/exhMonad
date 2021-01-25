{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Logic.Final where

import           Control.Applicative            ( Applicative(liftA2) )

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
type Atom = Int

-- An assignment is a function from variables to boolean values.
type Assignment = Atom -> Bool

-- 'at' maps variables to expressions of propositional logic, which compose via the boolean operators.
-- N.b., we don't need to define the boolean operators again for propositional logic.
class BoolSYM s => PropSYM s where
  at :: Atom -> s

-- Printing variables as ascii/unicode.
instance PropSYM A where
  at = A . show

instance PropSYM U where
  at = U . show

-- Interpretation parameterized to an assignment.
instance BoolSYM (Assignment -> Bool) where
  top  = pure top
  bot  = pure bot
  neg  = fmap neg
  conj = liftA2 conj

-- Variables are interpreted relative to an input assignment
instance PropSYM (Assignment -> Bool) where
  at n f = f n

-- evaluating formulas of propositional logic.
evalProp :: (Assignment -> Bool) -> Assignment -> Bool
evalProp = id

instance BoolSYM [Atom] where
  top  = []
  neg  = id
  conj = (++)

instance PropSYM [Atom] where
  at = pure

-- Computing the variables in a formula.
variables :: [Atom] -> [Atom]
variables = id








----------------------------------------------------
-- modular extension to an exhaustification logic --
----------------------------------------------------

newtype Exh = Exh (Assignment -> Bool)

instance (BoolSYM a, BoolSYM (Alts a)) => BoolSYM (a,Alts a,a) where
  top = (top, top, top)
  neg (a, bs, c) = (neg a, neg bs, neg c)
  disj (a, ps, c) (p, qs, t) =
    ( disj a  p
    , disj ps qs
    , (a `disj` p) `conj` foldr conj top (neg <$> (disj ps qs))
    )
  conj (a, ps, c) (p, qs, t) = (conj a p, conj ps qs, conj c t)

exh :: (BoolSYM a, BoolSYM (Alts a)) => (a, Alts a, a) -> a
exh (_, _, exh) = exh

tf4 = top `disj` top

-- >>> eval (exh tf4)
-- False


-- class PropSYM s => ExhSYM s where
--   exh :: s -> s

newtype Alts s = Alts [s] deriving (Functor,Applicative,Foldable)

instance BoolSYM a => BoolSYM (Alts a) where
  top = pure top
  bot = pure bot
  neg p = neg <$> p
  conj p q = liftA2 disj p q
  disj p q = liftA2 conj p q


tf3 = at 1 `disj` at 2

-- >>> viewA tf3
-- "(1|2)"

-- >>> viewA $ foldr conj top (neg <$> formalAlts tf3)
-- "(~(1&2)&T)"

-- >>>


-- >>> viewA $ top `conj` (at 1)
-- "(T&1)"

-- class PropSYM s where
--   top :: s
--   bot :: s
--   at :: Atom -> s
--   neg :: s -> s
--   conj :: s -> s -> s
--   disj :: s -> s -> s

-- type Viewable = String

-- instance PropSYM Viewable where
--   top = "T"
--   bot = "F"
--   at = show
--   neg = (++) "~"
--   conj t u = "(" ++ t ++ "&" ++ u ++ ")"
--   disj t u = "(" ++ t ++ "|" ++ u ++ ")"

-- view :: Viewable -> Viewable
-- view = id

-- tf1 :: PropSYM s => s
-- tf1 = at 1 `conj` top

-- instance PropSYM [Atom] where
--   top = []
--   bot = []
--   at = pure
--   neg = id
--   conj = (++)
--   disj = (++)

-- variables :: [Atom] -> [Atom]
-- variables = id

-- newtype Alt a = Alt [a] deriving (Functor,Applicative,Monad,Show,Foldable)

-- instance PropSYM a => PropSYM (Alt a) where
--   top = Alt [top]
--   bot = Alt [bot]
--   at n = Alt [at n]
--   neg (Alt alts) = Alt [neg alt | alt <- alts]
--   conj (Alt alts) (Alt alts') = Alt $ [disj psi psi' | psi <- alts, psi' <- alts']
--   disj (Alt alts) (Alt alts') = Alt $ [conj psi psi' | psi <- alts, psi' <- alts']

-- alts :: PropSYM a => Alt a -> Alt a
-- alts = id

-- instance PropSYM a => PropSYM (a,Alt a) where
--   top = (top,top)
--   bot = (bot,bot)
--   at n = (at n, at n)
--   neg (a,b) = (neg a, neg b)
--   conj (a,b) (c,d) = (conj a c, conj b d)
--   disj (a,b) (c,d) = (disj a c, disj b d)

-- pointed :: PropSYM a => (a, Alt a) -> (a, Alt a)
-- pointed = id


-- tf2 :: PropSYM s => s
-- tf2 = at 1 `disj` at 2 `disj` at 3

-- -- >>> view tf2
-- -- "((1|2)|3)"
-- -- >>> variables tf2
-- -- [1,2,3]
-- -- >>> view <$> alts tf2
-- -- Alt ["((1&2)&3)","((1&2)|3)","((1|2)&3)","((1|2)|3)","(1&3)","(1|3)","(2&3)","(2|3)","(1&2)","(1|2)","1","2","3"]

-- type Assignment = Int -> Bool

-- instance PropSYM (Assignment -> Bool) where
--   top = pure True
--   bot = pure False
--   at n g = g n
--   neg = fmap not
--   conj = liftA2 (&&)
--   disj = liftA2 (||)

-- instance PropSYM a => PropSYM (Assignment -> Bool,Alt a) where
--   top = (top,top)
--   bot = (bot,bot)
--   at n = (at n,at n)
--   neg (a,b) = (neg a, neg b)
--   conj (a,b) (c,d) = (conj a c, conj b d)
--   disj (a,b) (c,d) = (disj a c, disj b d)

-- eval :: (Assignment -> Bool) -> Assignment -> Bool
-- eval = id

-- g1 :: Assignment
-- g1 1 = True
-- g1 _ = False

-- ----------------------------------
-- -- extending the logic with exh --
-- ----------------------------------

-- class PropSYM s => ExhSYM s where
--   exh :: s -> s

-- instance ExhSYM a => ExhSYM (Alt a) where
--   exh (Alt alts) = Alt [exh alt | alt <- alts]

-- instance ExhSYM a => ExhSYM (a,Alt a) where
--   exh (p,qs) = (p,qs)

-- instance ExhSYM Viewable where
--   exh s = "Exh " ++ s

-- -- >>> view $ (fst $ pointed tf1)
-- -- "(1&T)"

-- conjoinAlts :: PropSYM b => Alt b -> b
-- conjoinAlts = foldr (conj) (top)

-- exhaust :: PropSYM a => (a, Alt a) -> (a, Alt a)
-- exhaust (p,q) = (p `conj` conjoinAlts (neg <$> q),q)

-- evalAlts :: Alt (Assignment -> Bool) -> Alt (Assignment -> Bool)
-- evalAlts = id

-- -- >>> view tf2
-- -- "((1|2)|3)"
-- -- >>> view <$> alts tf2
-- -- Alt ["((1&2)&3)"]
