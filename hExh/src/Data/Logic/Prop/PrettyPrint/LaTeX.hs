module Data.Logic.Prop.PrettyPrint.LaTeX (renderFormula) where

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath (wedge,vee)

import Data.Logic.Prop.Formula

neg :: LaTeXC l => l
neg = comm0 "neg"

toLaTeX :: Expr -> LaTeX
toLaTeX (Variable (Var v)) = fromString [v]
toLaTeX (Negation expr) = autoParens $ neg <> toLaTeX expr
toLaTeX (Conjunction expr1 expr2) = autoParens $ toLaTeX expr1 `wedge` toLaTeX expr2
toLaTeX (Disjunction expr1 expr2) = autoParens $ toLaTeX expr1 `vee` toLaTeX expr2

renderFormula :: Expr -> LaTeX
renderFormula = math . toLaTeX

-- >>> renderFormula $ mkVar 'a'
-- TeXMath Dollar (TeXRaw "a")
