module Data.Logic.Prop.StructuralAlts (subExprs, alts) where

import Data.Logic.Prop.Formula
import Control.Monad.Pointed

binaryConnectives :: [Expr -> Expr -> Expr]
binaryConnectives = [Conjunction, Disjunction]

subExprs :: Expr -> [Expr]
subExprs (Variable v) = [Variable v]
subExprs (Conjunction expr1 expr2) = (binaryConnectives <*> subExprs expr1 <*> subExprs expr2) ++ subExprs expr1 ++ subExprs expr2
subExprs (Disjunction expr1 expr2) = (binaryConnectives <*> subExprs expr1 <*> subExprs expr2) ++ subExprs expr1 ++ subExprs expr2
subExprs (Negation expr) = Negation <$> subExprs expr ++ subExprs expr

alts :: Expr -> Pointed Expr
alts expr = Pointed expr (subExprs expr)
