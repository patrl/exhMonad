module Data.Logic.Prop.StructuralAlts
  ( subExprs
  , alts
  )
where

import           Data.Logic.Prop.Formula
import           Control.Monad.Pointed

-- | In order to generate scalar alternatives, here we explicitly list the binary connectives.
binaryConnectives :: [Expr -> Expr -> Expr]
binaryConnectives = [Conjunction, Disjunction]

-- | Our datatype for wff of ExhProp represents formulas as trees.
-- This function recurses through a formula and does two things:
-- * It returns all subtrees which themselves correspond to a wff of exhProp.
-- * It returns all wff that we can get by replacing a binary connective with some other binary connective.
subExprs
  :: Expr -- ^ an input expression
  -> [Expr] -- ^ a set of alternatives
subExprs (Variable v) = pure $ Variable v
subExprs (Conjunction expr1 expr2) =
  (binaryConnectives <*> subExprs expr1 <*> subExprs expr2)
    ++ subExprs expr1
    ++ subExprs expr2
subExprs (Disjunction expr1 expr2) =
  (binaryConnectives <*> subExprs expr1 <*> subExprs expr2)
    ++ subExprs expr1
    ++ subExprs expr2
subExprs (Negation expr) = Negation <$> subExprs expr ++ subExprs expr
subExprs (Exh      expr) = Exh <$> subExprs expr ++ subExprs expr

-- | Takes an expression and returns a pair consisting of the expression and its alternatives.
alts :: Expr -> Pointed Expr
alts expr = Pointed expr (subExprs expr)
