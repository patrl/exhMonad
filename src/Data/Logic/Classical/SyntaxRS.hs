{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Logic.Classical.SyntaxRS where

import           Control.Applicative            ( Applicative(liftA2) )
import           Control.Monad                  ( replicateM )
import           Data.Functor.Foldable          (Recursive(cata) )
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.IntMap.Strict            as IntM
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import Data.Maybe (fromJust)

-- A datatype for unary operators.
data UOp = Neg | Exh | Box | Loz
    deriving Eq

-- A datatype for binary operators
data BOp = Conj | Disj deriving (Eq)

-- A recursive datatype for boolean/propositional formulas.
data Expr a = Simple a | Unary UOp (Expr a) | Binary BOp (Expr a) (Expr a) deriving (Eq,Foldable)

makeBaseFunctor ''Expr

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
    show Exh = "O"
    show Box = "[]"
    show Loz = "<>"

asciiU :: UOp -> String
asciiU Neg = "~"
asciiU Exh = "𝒪"
asciiU Box = "□"
asciiU Loz = "◇"

asciiB :: BOp -> String
asciiB Conj = "&"
asciiB Disj = "|"

viewA :: Expr Var -> String
viewA = cata $ \case
    (SimpleF (Var n) ) -> show n
    (UnaryF op ps    ) -> asciiU op ++ ps
    (BinaryF op ps qs) -> "(" ++ ps ++ asciiB op ++ qs ++ ")"

semU :: UOp -> Bool -> Bool
semU Neg = not
semU _ = id
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
tf1 = Binary Disj (toExpr 1) (Binary Disj (toExpr 2) (Binary Disj (toExpr 3) (toExpr 4)))

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

altScalar :: PropExpr -> [PropExpr]
altScalar = cata $ \case
  (SimpleF v) -> pure $ Simple v
  (UnaryF op as) -> Unary op <$> as
  (BinaryF _ as bs) -> concat [liftA2 (Binary op) as bs | op <- [Conj,Disj]]

altSauerland :: PropExpr -> (PropExpr,[PropExpr])
altSauerland = cata $ \case
  (SimpleF v) -> (Simple v, pure $ Simple v)
  (UnaryF op (a,as)) -> (Unary op a, Unary op <$> as)
  (BinaryF op (a,as) (b,bs)) -> (Binary op a b,(concat [liftA2 (Binary op') as bs | op' <- [Conj,Disj]]) <> as <> bs)

-- pushNegsIn :: PropExpr -> PropExpr
-- pushNegsIn = para $ \case
--   SimpleF v -> Simple v
--   (UnaryF Neg ((Binaru Disj _ _),(Binary Disj _ _))) -> undefined
--   (UnaryF Neg (Binary Conj expr expr',_)) -> undefined
--   (UnaryF Neg (expr,_)) -> undefined
--   (BinaryF op (expr,_) (expr',_)) -> undefined

-- It would be nice to be able to factor the recursion out.
pushNegsIn :: PropExpr -> PropExpr
pushNegsIn = cata $ \case
  SimpleF v -> Simple v
  UnaryF Neg (Binary Disj a b) -> Binary Conj (pushNegsIn (Unary Neg a)) (pushNegsIn (Unary Neg b))
  UnaryF Neg (Binary Conj a b) -> Binary Disj (pushNegsIn (Unary Neg a)) (pushNegsIn (Unary Neg b))
  UnaryF op expr -> Unary op expr
  BinaryF op expr expr' -> Binary op expr expr'

tf3 :: Expr Var
tf3 = Unary Neg tf1

-- >>> altSauerland tf3
-- (~(1|(2|(3|4))),[~(1&(2&(3&4))),~(1&(2&(3|4))),~(1&(2&3)),~(1&(2&4)),~(1&(2|(3&4))),~(1&(2|(3|4))),~(1&(2|3)),~(1&(2|4)),~(1&2),~(1&(3&4)),~(1&(3|4)),~(1&3),~(1&4),~(1|(2&(3&4))),~(1|(2&(3|4))),~(1|(2&3)),~(1|(2&4)),~(1|(2|(3&4))),~(1|(2|(3|4))),~(1|(2|3)),~(1|(2|4)),~(1|2),~(1|(3&4)),~(1|(3|4)),~(1|3),~(1|4),~1,~(2&(3&4)),~(2&(3|4)),~(2&3),~(2&4),~(2|(3&4)),~(2|(3|4)),~(2|3),~(2|4),~2,~(3&4),~(3|4),~3,~4])

-----------------
-- modal logic --
-----------------

-- evalExh :: PropExpr -> Proposition
-- evalExh = cata $ \case
--   (SimpleF (Var n)      ) -> IntM.lookup n
--   (UnaryF Exh p) ->
--   (UnaryF op p          ) -> (fmap . fmap) (semU op) p
--   (BinaryF op expr expr') -> (liftA2 . liftA2 $ semB op) expr expr'
