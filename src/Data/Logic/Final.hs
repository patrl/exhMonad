{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Logic.Final where

import Data.Functor.Identity (Identity)
import Control.Applicative ( Applicative(liftA2) )
import           Text.Parsec.Language           ( emptyDef )
import Text.ParserCombinators.Parsec
    ( (<|>), parse, ParseError, Parser, digit )
import Text.ParserCombinators.Parsec.Expr
    ( Assoc(AssocRight),
      buildExpressionParser,
      Operator(Prefix, Infix) )
import qualified Text.ParserCombinators.Parsec.Token
                                               as P
import Text.Parsec.Combinator ( many1 )


type Atom = Int

class PropSYM s where
  top :: s
  bot :: s
  at :: Atom -> s
  neg :: s -> s
  conj :: s -> s -> s
  disj :: s -> s -> s

type Viewable = String

instance PropSYM Viewable where
  top = "T"
  bot = "F"
  at = show
  neg = (++) "~"
  conj t u = "(" ++ t ++ "&" ++ u ++ ")"
  disj t u = "(" ++ t ++ "|" ++ u ++ ")"

view :: Viewable -> Viewable
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

newtype Alt a = Alt [a] deriving (Functor,Applicative,Monad,Show,Foldable)

instance PropSYM a => PropSYM (Alt a) where
  top = Alt [top]
  bot = Alt [bot]
  at n = Alt [at n]
  neg (Alt alts) = Alt [neg alt | alt <- alts]
  conj (Alt alts) (Alt alts') = Alt $ [disj psi psi' | psi <- alts, psi' <- alts']
  disj (Alt alts) (Alt alts') = Alt $ [conj psi psi' | psi <- alts, psi' <- alts']

alts :: PropSYM a => Alt a -> Alt a
alts = id

instance PropSYM a => PropSYM (a,Alt a) where
  top = (top,top)
  bot = (bot,bot)
  at n = (at n, at n)
  neg (a,b) = (neg a, neg b)
  conj (a,b) (c,d) = (conj a c, conj b d)
  disj (a,b) (c,d) = (disj a c, disj b d)

pointed :: PropSYM a => (a, Alt a) -> (a, Alt a)
pointed = id



-- >>> variables tf1
-- [1]

-- >>> view <$> alts tf1
-- Alt ["(1|T)"]
-- >>> ($ g1) <$> eval <$> alts tf1
-- Alt [True]
--

--   (maybe you haven't applied a function to enough arguments?)

-- >>> view . fst $ pointed tf1
-- "(1&T)"
-- >>> view <$> (snd $ pointed tf1)
-- Alt ["(1|T)"]

-- >>> view <$> alt

tf2 :: PropSYM s => s
tf2 = at 1 `disj` at 2 `disj` at 3

-- >>> view tf2
-- "((1|2)|3)"
-- >>> variables tf2
-- [1,2,3]
-- >>> view <$> alts tf2
-- Alt ["((1&2)&3)","((1&2)|3)","((1|2)&3)","((1|2)|3)","(1&3)","(1|3)","(2&3)","(2|3)","(1&2)","(1|2)","1","2","3"]

type Assignment = Int -> Bool

instance PropSYM (Assignment -> Bool) where
  top = pure True
  bot = pure False
  at n g = g n
  neg = fmap not
  conj = liftA2 (&&)
  disj = liftA2 (||)

instance PropSYM a => PropSYM (Assignment -> Bool,Alt a) where
  top = (top,top)
  bot = (bot,bot)
  at n = (at n,at n)
  neg (a,b) = (neg a, neg b)
  conj (a,b) (c,d) = (conj a c, conj b d)
  disj (a,b) (c,d) = (disj a c, disj b d)

eval :: (Assignment -> Bool) -> Assignment -> Bool
eval = id

g1 :: Assignment
g1 1 = True
g1 _ = False

----------------------------------
-- extending the logic with exh --
----------------------------------

class PropSYM s => ExhSYM s where
  exh :: s -> s

instance ExhSYM a => ExhSYM (Alt a) where
  exh (Alt alts) = Alt [exh alt | alt <- alts]

instance ExhSYM a => ExhSYM (a,Alt a) where
  exh (p,qs) = (p,qs)

instance ExhSYM Viewable where
  exh s = "Exh " ++ s

-- >>> view $ (fst $ pointed tf1)
-- "(1&T)"

conjoinAlts :: PropSYM b => Alt b -> b
conjoinAlts qs = foldr (conj) (top) qs

exhaust :: PropSYM a => (a, Alt a) -> (a, Alt a)
exhaust (p,q) = (p `conj` conjoinAlts (neg <$> q),q)

evalAlts :: Alt (Assignment -> Bool) -> Alt (Assignment -> Bool)
evalAlts = id

-- >>> view tf2
-- "((1|2)|3)"
-- >>> view <$> alts tf2
-- Alt ["((1&2)&3)"]
