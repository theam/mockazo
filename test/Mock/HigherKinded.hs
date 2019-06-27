module Mock.HigherKinded
  ( Action(..)
  , Component(..)
  , mock
  , test
  ) where

import Relude
import Data.Component.Mock

data Component context = Component
  { foo :: Maybe Int -> Maybe Int -> context ()
  , bar :: Maybe Text -> context (Maybe Int)
  }

makeMock ''Component

test :: Monad context => Component context -> context ()
test Component{..} = do
  x <- bar (Just "x")
  y <- bar (Just "y")
  foo x y