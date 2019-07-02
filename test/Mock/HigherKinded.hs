module Mock.HigherKinded
  ( Action(..)
  , Component(..)
  , mock
  , test
  ) where

import Relude
import Data.Component.Mock

data Wat a b = Wat
    deriving (Eq, Show)

data Watson
data Sherlock

data Component context = Component
  { foo :: Maybe Int -> Maybe Int -> context ()
  , bar :: Maybe Text -> context (Maybe Int)
  , quux :: Wat Watson Sherlock -> context () -> context ()
  }

makeMock ''Component

test :: Monad context => Component context -> context ()
test Component{..} = do
  x <- bar (Just "x")
  y <- bar (Just "y")
  foo x y