module Mock.SingleKinded
  ( Action(..)
  , Component(..)
  , mock
  , test
  ) where

import Relude
import Data.Component.Mock

data Component context = Component
  { foo :: Int -> Int -> context ()
  , bar :: Text -> context Int
  }

makeMock ''Component

test :: Monad context => Component context -> context ()
test Component{..} = do
  x <- bar "x"
  y <- bar "y"
  foo x y