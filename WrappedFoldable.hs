module WrappedFoldable where

import Control.Monad
import Data.Foldable

newtype WrappedFoldable f a =
  WrappedFoldable { unwrap :: f a } deriving Show

instance Monad f => Functor (WrappedFoldable f) where
  fmap f (WrappedFoldable a) = WrappedFoldable (liftM f a)


{-
instance Monad f => Monad (WrappedFoldable f) where
  m >>= g = (WrappedFoldable (unwrap m)) >>= g
  return = WrappedFoldable . return


instance Monad f => Monad (WrappedFoldable f) where
  (WrappedFoldable m) >>= f = (WrappedFoldable m) >>= f
  return = WrappedFoldable . return

-}
instance Monad f => Monad (WrappedFoldable f) where
  m >>= f = m >>= f
  return = return

mything = WrappedFoldable [1,2,3]

myfunc :: Int -> Maybe Int
myfunc 3 = Just 5
myfunc _ = Nothing

