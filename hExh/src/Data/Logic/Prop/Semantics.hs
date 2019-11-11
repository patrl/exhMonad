-- | semantics for propositional logic
module Data.Logic.Prop.Semantics where

import           Prelude                 hiding ( lookup )
import           Data.Logic.Prop.Formula
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                )
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( replicateM )

-- An assignment is a mapping from variables to truth-values
type Assignment = Map Var Bool

-- lists the variables in a formula
variables :: Expr -> [Var]
variables expr =
  let vars_ (Variable v       ) vs = v : vs
      vars_ (Negation e       ) vs = vars_ e vs
      vars_ (Conjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Disjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
  in  map head . group . sort $ vars_ expr []

universe :: [Var] -> [Assignment]
universe vs =
  map (fromList . zip vs) $ replicateM (length vs) [True, False]

assignments :: Expr -> [Assignment]
assignments expr =
  let vs = variables expr
      ps = replicateM (length vs) [True, False]
  in  map (fromList . zip vs) ps

interpret :: Expr -> Assignment -> Bool
interpret (Variable v           ) vs = fromMaybe False (lookup v vs)
interpret (Negation expr        ) vs = not $ interpret expr vs
interpret (Conjunction exp1 exp2) vs = interpret exp1 vs && interpret exp2 vs
interpret (Disjunction exp1 exp2) vs = interpret exp1 vs || interpret exp2 vs

values :: Expr -> [Bool]
values expr = map (interpret expr) (assignments expr)

-- | Determines whether an expression is contradictory.
isContradiction :: Expr -> Bool
isContradiction = not . or . values

proposition :: Expr -> [Assignment] -> [Assignment]
proposition expr model = [w | w <- model, interpret expr w]

entails :: Expr -> Expr -> Bool
expr `entails` expr' = all (`elem` proposition expr' (universe (variables expr ++ variables expr'))) (proposition expr (universe (variables expr ++ variables expr')))
