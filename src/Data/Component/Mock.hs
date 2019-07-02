module Data.Component.Mock
  ( (:~:)(..)
  , WithResult(..)
  , IsAction(..)
  , mockAction
  , InContextOf
  , Executes
  , runMock
  , withActions

  , module Data.Component.Mock.TH
  , module Data.Component.Mock.Match
  ) where

import Relude
import Data.Type.Equality ((:~:)(..))
import Data.Constraint ((:-), (\\))
import Data.Constraint.Forall (ForallF, instF)
import Control.Monad.Trans.MultiState hiding (MultiState)
import Data.HList.ContainsType

import Data.Component.Mock.TH
import Data.Component.Mock.Match

{- | Context that gets injected to the components
to store the values that are being executed.

These actions will be compared during the tests
with the ones that you expect, failing in case
of a mismatch.
-}
type InContextOf s =
  MultiStateT s IO

{-| Constraint to specify that some actions
contain the type of actions to be executed.
-}
type Executes action actions =
  ContainsType [WithResult action] actions

{-| Runs the mock context and all the checks
with it
-}
runMock :: InContextOf '[] a -> IO ()
runMock = runMultiStateTNil_

{-| Specify the expected actions for a given component
to expect to be executed
-}
withActions
  :: IsAction action
  => [WithResult action]
  -> InContextOf ([WithResult action] : otherActions) a
  -> InContextOf otherActions (a, [WithResult action])
withActions actions execution = do
  (result, actionsRest) <- withMultiState actions execution
  case actionsRest of
    [] ->
      pure (result, actionsRest)

    remainingActions ->
      error
        $ "Execution ended, but those actions were expected to be run:\n"
        <> unlines (fmap (\(action :-> _) -> "  • '" <> showAction action <> "'") remainingActions)

{-| Operator to specify that an action returns some result
-}
data WithResult action where
  (:->) :: action result -> result -> WithResult action

{-| Class that all actions must implement in order to work
with the rest of the library.

You only need to implement 'eqAction'
-}
class IsAction (action :: Type -> Type) where
  eqAction :: action a -> action b -> Maybe (a :~: b)
  showAction :: action a -> Text

  default showAction :: ForallF Show action => action a -> Text
  showAction =
    toText . showAction'
   where
    showAction' :: forall g a. ForallF Show g => g a -> String
    showAction' x = show x \\ (instF :: ForallF Show g :- Show (g a))

{-| Utility function to be used in the creation of the mock
components, so the methods store action values instead of
executing anything else
-}
mockAction
  :: ContainsType ([WithResult action]) actions
  => IsAction action
  => Text
  -> action result
  -> InContextOf actions result
mockAction functionName action = do
  nextAction <- mGet
  case nextAction of
    [] ->
      error
        $ "Expected end of program, but called '" <> functionName <> "'\n"
        <> "  given action: '" <> showAction action <> "'\n"

    (action' :-> result) : actions
     | Just Refl <- action `eqAction` action' -> do
        mSet actions
        pure result

     | otherwise ->
        error
          $ "Incorrect call to '" <> functionName <> "'\n"
          <> "  called: '" <> showAction action <> "'\n"
          <> "  expected a call to: '" <> showAction action' <> "'\n"
