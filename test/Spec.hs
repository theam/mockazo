import Relude

import Test.Hspec
import Data.Component.Mock

import qualified Mock.SingleKinded as SingleKinded
import qualified Mock.HigherKinded as HigherKinded

main :: IO ()
main = hspec $ do
  describe "mockazo" $ do
    describe "mocking components" $ do
      it "handles single kinded types" $ do
        let mock = SingleKinded.mock
        runMock
          $ withActions
            [ SingleKinded.Bar "x" :-> 3
            , SingleKinded.Quux :-> 4
            , SingleKinded.Foo 3 4 :-> ()
            ]
          $ SingleKinded.test mock

      it "handles (fully applied) higher kinded types" $ do
        let mock = HigherKinded.mock
        runMock
          $ withActions
            [ HigherKinded.Bar (Just "x") :-> Just 2
            , HigherKinded.Bar (Just "y") :-> Just 3
            , HigherKinded.Foo (Just 2) (Just 3) :-> ()
            ]
          $ HigherKinded.test mock
