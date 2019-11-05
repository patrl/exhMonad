module Data.Logic.ExhProp.Semantics where

import           Prelude                 hiding ( lookup )

import           Data.Logic.ExhProp.Formula
import           Data.Logic.ExhProp.Alternatives
                                                ( )
import           Data.Map                       ( Map
                                                , fromList
                                                , lookup
                                                )
import           Data.List                      ( group
                                                , sort
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( replicateM )

-- A mapping assigns a truth-value to a variable.
type Mapping = Map Var Bool

variables :: Expr -> [Var]
variables expr =
  let vars_ (Variable v       ) vs = v : vs
      vars_ (Negation e       ) vs = vars_ e vs
      vars_ (Exh      e       ) vs = vars_ e vs
      vars_ (ExhIE    e       ) vs = vars_ e vs
      vars_ (Conjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
      vars_ (Disjunction e1 e2) vs = vars_ e1 vs ++ vars_ e2 vs
  in  map head . group . sort $ vars_ expr []

assignments :: Expr -> [Mapping]
assignments expr =
  let vs = variables expr
      ps = replicateM (length vs) [True, False]
  in  map (fromList . zip vs) ps

interpret :: Expr -> Mapping -> Bool
interpret (Variable v           ) vs = fromMaybe False (lookup v vs)
interpret (Negation expr        ) vs = not $ interpret expr vs
-- interpret (Exh      expr        ) vs = implicate expr (exclAlts expr) vs
-- interpret (ExhIE expr) vs = implicate expr (exclAltsIE expr) vs
interpret (Conjunction exp1 exp2) vs = interpret exp1 vs && interpret exp2 vs
interpret (Disjunction exp1 exp2) vs = interpret exp1 vs || interpret exp2 vs
