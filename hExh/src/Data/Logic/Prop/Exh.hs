module Data.Logic.Prop.Exh where

import           Data.Logic.Prop.Formula
import           Control.Monad.Pointed
import           Data.Logic.Prop.Semantics

filterNonWeaker :: Pointed Expr -> Pointed Expr
filterNonWeaker (Pointed expr alts) = Pointed
  expr
  (filter (\p -> not . isContradiction $ expr `Conjunction` Negation p) alts)

exh :: Pointed Expr -> Assignment -> Bool
exh (Pointed expr alts) vs = foldr ((&&) . not . (`interpret` vs)) (interpret expr vs) alts

-- test
aOrBorC :: Expr
aOrBorC = mkVar 'a' `Disjunction` mkVar 'b' `Disjunction` mkVar 'c'

