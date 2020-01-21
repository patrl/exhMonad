-- | semantics for propositional logic
module Data.Logic.Prop.Semantics where

import           Prelude                 hiding ( lookup )
import           Data.Logic.Prop.Formula
import           Data.Logic.Prop.StructuralAlts
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                )
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Pointed

-- | An assignment is a mapping from variables to truth-values
type Assignment = Map Var Bool

-- | lists the variables in a formula
variables :: Expr -> [Var]
variables expr =
  let vars_ (Variable v       ) vs = v : vs
      vars_ (Negation e       ) vs = vars_ e vs
      vars_ (Exh      e       ) vs = vars_ e vs
      vars_ (Conjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Disjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
  in  map head . group . sort $ vars_ expr []

-- | Lists all the possible assignments, given a list of variables.
universe :: [Var] -> [Assignment]
universe vs = map (fromList . zip vs) $ replicateM (length vs) [True, False]

-- | Lists all the possible assignments, given the variables in the formula.
assignments :: Expr -> [Assignment]
assignments expr =
  let vs = variables expr
      ps = replicateM (length vs) [True, False]
  in  map (fromList . zip vs) ps

-- | Interpets an expression relative to an assignment
interpret :: Expr -> Assignment -> Bool
interpret (Variable v           ) vs = fromMaybe False (lookup v vs)
interpret (Negation expr        ) vs = not $ interpret expr vs
interpret (Exh      expr        ) vs = exhMw (alts expr) vs
interpret (Conjunction exp1 exp2) vs = interpret exp1 vs && interpret exp2 vs
interpret (Disjunction exp1 exp2) vs = interpret exp1 vs || interpret exp2 vs

values :: Expr -> [Bool]
values expr = map (interpret expr) (assignments expr)

-- | Determines whether an expression is contradictory.
-- isContradiction :: Expr -> Bool
-- isContradiction = not . or . values

proposition :: Expr -> [Assignment] -> [Assignment]
proposition expr model = [ w | w <- model, interpret expr w ]

-- This can probably be made more concise
entails :: Expr -> Expr -> Bool
expr `entails` expr' = all
  (`elem` proposition expr' (universe (variables expr ++ variables expr')))
  (proposition expr (universe (variables expr ++ variables expr')))

-- | defines an ordering on assignments (i.e., worlds) relative to a set of alternatives. More minimal assignments make more alternatives false.
preorder :: [Expr] -> Assignment -> Assignment -> Ordering
preorder exprs u v | uAlts `isPropersubset` vAlts = LT
                   | vAlts `isPropersubset` uAlts = GT
                   | otherwise                    = EQ
 where
  uAlts = [ p | p <- exprs, interpret p u ]
  vAlts = [ q | q <- exprs, interpret q v ]

-- | Performs minimal worlds exhaustification
exhMw :: Pointed Expr -> Assignment -> Bool
exhMw (Pointed p exprs) w = interpret p w && null
  [ v | v <- assignments p, interpret p v && preorder exprs v w == LT ]

isPropersubset :: Eq a => [a] -> [a] -> Bool
isPropersubset x y = all (`elem` y) x && x /= y

-- >>> subExprs (mkVar 'a' `Disjunction` mkVar 'b')
-- [a,b]

-- >>> values (Exh (mkVar 'a' `Disjunction` mkVar 'b'))
-- [False,True,True,False]
