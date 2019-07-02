module Data.Component.Mock.Match
  ( Match(..)
  , Typeable(..)
  ) where

import Relude
import qualified Data.Typeable as Typeable

data AnyFunction
data AnyAction

data Foo a b

anyFunction :: Foo AnyFunction a
anyFunction = undefined

anyAction :: Foo AnyAction a
anyAction = undefined

class Match a b where
  (<=>) :: a -> b -> Bool

-- instance {-# OVERLAPS #-} (a ~ b, Eq a) => Match a b where
--   (<=>) = (==)

instance {-# OVERLAPPABLE #-} Match (context1 a) (g b) where
  _ <=> _ = True

---------

class IsHigherKinded a
instance IsHigherKinded (f a)
instance IsHigherKinded (f a b)
instance IsHigherKinded (f a b c)
instance IsHigherKinded (f a b c d)

class IsFunction a
instance IsFunction (a -> b)
instance IsFunction (a -> b -> c)
instance IsFunction (a -> b -> c -> d)
instance IsFunction (a -> b -> c -> d -> e)
instance IsFunction (a -> b -> c -> d -> e -> f)
instance IsFunction (a -> b -> c -> d -> e -> f -> g)
instance IsFunction (a -> b -> c -> d -> e -> f -> g -> h)
instance IsFunction (a -> b -> c -> d -> e -> f -> g -> h -> i)
instance IsFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> j)
instance IsFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
instance IsFunction (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l)