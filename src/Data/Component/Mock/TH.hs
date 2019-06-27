module Data.Component.Mock.TH
  (makeMock) where

import Relude

import qualified Language.Haskell.TH as Meta

import qualified Data.Component.Mock.TH.Gadt as Gadt
import qualified Data.Component.Mock.TH.Instance as Instance
import qualified Data.Component.Mock.TH.NewFunction as NewFunction

makeMock :: Meta.Name -> Meta.DecsQ
makeMock componentName = do
  info <- Meta.reify componentName
  case info of
    Meta.TyConI (Meta.DataD _ recordName _ _ [Meta.RecC _ fields] _) -> do
      action <- Gadt.make fields
      instance' <- Instance.make fields
      newFunction <- NewFunction.make recordName fields
      pure $ action <> [instance'] <> newFunction

    _ ->
      fail "makeMock only accepts simple records"
