-- | syntax for wff of propositional logic
module Data.Logic.Prop.Formula where

-- | Our datatype for variables.
newtype Var = Var Char
    deriving (Eq, Ord)

-- | A helper function for turning characters into variables.
mkVar :: Char -> Expr
mkVar = Variable . Var

-- | Our datatype for wff of exhProp (propositional logic with an exhaustivity operator).
-- N.b. that we treat the exhaustivity operator as an expression of the object language.
data Expr = Variable Var
          | Negation Expr
          | Exh Expr
          | Conjunction Expr Expr
          | Disjunction Expr Expr
          deriving Eq

instance Show Var where
  show (Var v) = [v]

showBinaryConnective :: (Expr -> String) -> String -> Expr -> Expr -> String
showBinaryConnective show_ symbol exp1 exp2 =
  '(' : show_ exp1 ++ " " ++ symbol ++ " " ++ show_ exp2 ++ ")"

showBC :: String -> Expr -> Expr -> String
showBC = showBinaryConnective show

instance Show Expr where
  show (Variable name        ) = show name
  show (Negation expr        ) = '~' : show expr
  show (Exh      expr        ) = 'O' : show expr
  show (Conjunction exp1 exp2) = showBC "&&" exp1 exp2
  show (Disjunction exp1 exp2) = showBC "||" exp1 exp2
