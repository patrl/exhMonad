module Control.Monad.Pointed where

import           Control.Monad                  ( liftM
                                                , ap
                                                )

data Pointed a = Pointed a [a]
  deriving (Eq, Ord, Show, Read)

toList :: Pointed a -> [a]
toList (Pointed x xs) = x : xs

instance Functor Pointed where
  fmap = liftM

instance Applicative Pointed where
  pure  = return
  (<*>) = ap

instance Monad Pointed where
  return x = Pointed x []
  Pointed x xs >>= k = Pointed y (ys ++ (xs >>= toList . k))
    where Pointed y ys = k x
