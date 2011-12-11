module Pantry.WrappedFoldable where

import Prelude((.), ($))
import Control.Monad
import qualified Data.Foldable as F
import Control.Applicative

newtype WrappedFoldable f a =
  WrappedFoldable { unwrap :: f a }

instance Monad f => Functor (WrappedFoldable f) where
  fmap f (WrappedFoldable a) = WrappedFoldable (liftM f a)

instance Monad m => Applicative (WrappedFoldable m) where
    pure = WrappedFoldable . return
    WrappedFoldable f <*> WrappedFoldable v = WrappedFoldable (f `ap` v)

instance F.Foldable f => F.Foldable (WrappedFoldable f) where
  foldr f b = F.foldr f b . unwrap

