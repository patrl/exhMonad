{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}

module Data.Logic.Classical.SyntaxRS where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad                  ( replicateM )
import           Data.Functor.Foldable          ( Base
                                                , Corecursive(embed)
                                                , Recursive(cata, project)
                                                )
import qualified Data.IntMap.Strict            as IntM
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S

-- A recursive datatype for boolean/propositional formulas.
data Expr a = Simple a | Unary UOp (Expr a) | Binary BOp (Expr a) (Expr a) deriving (Eq,Foldable)

-- A datatype for unary operators.
data UOp = Neg
    deriving Eq

-- A datatype for binary operators
data BOp = Conj | Disj deriving (Eq)

-- We treat variables as a wrapper around the built-in 'Int' type.
newtype Var = Var Int deriving (Eq,Ord)

instance Show Var where
    show (Var n) = show n

toExpr :: Int -> Expr Var
toExpr = Simple . Var

type PropExpr = Expr Var

type BoolExpr = Expr Bool

instance Show BOp where
    show Conj = "&"
    show Disj = "|"

instance Show UOp where
    show Neg = "~"

-- An Abstract Syntax Tree for logical expressions
data ExprF a r = SimpleF a | UnaryF UOp r | BinaryF BOp r r deriving (Functor)

type instance Base (Expr a) = ExprF a

instance Recursive (Expr a) where
    project (Simple a            ) = SimpleF a
    project (Unary op expr       ) = UnaryF op expr
    project (Binary op expr expr') = BinaryF op expr expr'

instance Corecursive (Expr a) where
    embed (SimpleF v            ) = Simple v
    embed (UnaryF op expr       ) = Unary op expr
    embed (BinaryF op expr expr') = Binary op expr expr'

asciiU :: UOp -> [Char]
asciiU Neg = "~"

asciiB :: BOp -> [Char]
asciiB Conj = "&"
asciiB Disj = "|"

viewA :: Expr Var -> String
viewA = cata $ \case
    (SimpleF (Var n) ) -> show n
    (UnaryF op ps    ) -> asciiU op ++ ps
    (BinaryF op ps qs) -> "(" ++ ps ++ asciiB op ++ qs ++ ")"

semU :: UOp -> Bool -> Bool
semU Neg = not
semB :: BOp -> Bool -> Bool -> Bool
semB Conj = (&&)
semB Disj = (||)

evalB :: BoolExpr -> Bool
evalB = cata $ \case
    (SimpleF t     ) -> t
    (UnaryF op t   ) -> semU op t
    (BinaryF op t u) -> semB op t u

evalP :: PropExpr -> Proposition
evalP = cata $ \case
    (SimpleF (Var n)      ) -> IntM.lookup n
    (UnaryF op p          ) -> (fmap . fmap) (semU op) p
    (BinaryF op expr expr') -> (liftA2 . liftA2 $ semB op) expr expr'

type Assignment = IntM.IntMap Bool
type Proposition = Assignment -> Maybe Bool

instance Show PropExpr where
    show expr = viewA expr

tf1 :: Expr Var
tf1 = Binary Disj (toExpr 1) (toExpr 2)

tf2 :: Expr Var
tf2 = toExpr 1

variables :: Expr Var -> S.Set Var
variables = cata $ \case
    (SimpleF v      ) -> S.singleton v
    (UnaryF _ vs    ) -> vs
    (BinaryF _ vs us) -> vs `S.union` us

universe :: S.Set Var -> [Assignment]
universe vs =
    [ IntM.fromList [ (k, t) | (Var k) <- S.toList vs | t <- ts ]
    | ts <- replicateM (length vs) [True, False]
    ]

-- We can use this, if, for whatever reason, we want the intermediate results
universe' :: PropExpr -> S.Set Assignment
universe' = cata $ \case
  -- The algebra
    (SimpleF (Var k)) -> S.fromList [ IntM.singleton k t | t <- [True, False] ]
    (UnaryF _ gs    ) -> gs
    (BinaryF _ gs hs) ->
        S.fromList [ g `IntM.union` h | g <- S.toList gs, h <- S.toList hs ]

truthTable' :: PropExpr -> M.Map Assignment Bool
truthTable' = cata $ \case
  -- the algebra
  (SimpleF (Var k)) -> M.fromList [ (IntM.singleton k t,t) | t <- [True, False] ]
  (UnaryF op ms) -> semU op <$> ms
  (BinaryF op ms ns) -> M.fromList [ (g `IntM.union` h,semB op t u) | (g,t) <- M.toList ms, (h,u) <- M.toList ns]
