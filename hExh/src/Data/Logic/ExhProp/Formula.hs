module Data.Logic.ExhProp.Formula where

newtype Var = Var Char
    deriving (Eq, Ord)

mkVar :: Char -> Expr
mkVar = Variable . Var

data Expr = Variable Var
          | Negation Expr
          | Exh Expr
          | ExhIE Expr
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
  show (Exh      expr        ) = "exh " ++ show expr
  show (ExhIE    expr        ) = "exh-ie " ++ show expr
  show (Conjunction exp1 exp2) = showBC "&&" exp1 exp2
  show (Disjunction exp1 exp2) = showBC "||" exp1 exp2
