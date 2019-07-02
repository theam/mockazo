module Data.Component.Mock.TH.Gadt
  ( make
  ) where

import Relude
import qualified Relude.Unsafe as Unsafe

import qualified Language.Haskell.TH as Meta
import qualified Language.Haskell.TH.Syntax as Meta

import Data.Component.Mock.TH.Common

{-| Generates a GADT definition for actions for
a record of functions.

For example, for a record like

@
data Component context = Component
  { info    :: Text -> context ()
  , success :: Text -> context ()
  , fail    :: Text -> context ()
  , debug   :: Text -> context ()
  , start   :: Text -> context ()
  , ask     :: Text -> Text -> context Text
  }
@

it will generate the actions:

@
data Action a where
  Info    :: Text -> Action ()
  Success :: Text -> Action ()
  Fail    :: Text -> Action ()
  Debug   :: Text -> Action ()
  Start   :: Text -> Action ()
  Ask     :: Text -> Text -> Action Text
@
-}
make :: [Meta.VarBangType] -> Meta.DecsQ
make fields = do
  constructors <- traverse constructorFromField fields
  let gadtDefinition = defaultDefinition constructors
  pure (gadtDefinition : derivedInstances)

-- | Generates a GADT constructor based on a record field
constructorFromField :: Meta.VarBangType -> Meta.Q Meta.Con
constructorFromField (fieldName, _, type') =
  case functionTypeToList type' of
    result : [] -> do
      resultType <- substituteReturnContext result
      let name = titleizeName fieldName
      pure $ Meta.GadtC [name] [] resultType
    othersAndResult -> do
      let resultType = Unsafe.last othersAndResult
      let inputType  = Unsafe.init othersAndResult
      resultType <- substituteReturnContext resultType
      let name = titleizeName fieldName
      let res = Meta.GadtC [name] (toList $ fmap (noBang,) inputType) resultType
                -- & addTypeableConstraint inputType
      traceM "=====  BASE TYPE GOT IS :  ====="
      traceShowM type'
      traceM "===== GENERATING TYPE FROM ====="
      traceShowM res
      traceM "=====   GENERATED TYPE IS  ====="
      traceM $ Meta.pprint res
      pure res

addTypeableConstraint :: [Meta.Type] -> Meta.Con -> Meta.Con
addTypeableConstraint types constructor =
  Meta.ForallC
    (fmap Meta.PlainTV vars)
    (fmap typeableConstraint vars)
    constructor
 where
  vars =
    types >>= extractVar
  extractVar (Meta.AppT t1 t2) =
    extractVar t1 <> extractVar t2
  extractVar (Meta.VarT var) =
    [var]
  extractVar _ =
    []
  typeableConstraint a =
    Meta.AppT (Meta.ConT $ Meta.mkName "Typeable") (Meta.VarT a)


derivedInstances :: [Meta.Dec]
derivedInstances =
  fmap deriveInstanceFor ["Eq", "Show"]
 where
  deriveInstanceFor classStr =
    Meta.StandaloneDerivD
      Nothing
      []
      (Meta.AppT
        (Meta.ConT $ Meta.mkName classStr)
        (Meta.AppT
          (Meta.ConT actionName)
          (Meta.VarT $ Meta.mkName "r")))

-- | Substitutes the context type variable by Action
substituteReturnContext :: Meta.Type -> Meta.Q Meta.Type
substituteReturnContext resultType =
  case resultType of
    Meta.AppT (Meta.VarT _) res ->
      pure $ Meta.AppT (Meta.ConT actionName) res

    other -> do
      fail
        $ "Data.Component.Mock only works with records that return values in a context, but got:\n"
        <> "  - " <> Meta.pprint other

-- | Omit strictness and unpacking
noBang :: Meta.Bang
noBang = Meta.Bang Meta.NoSourceUnpackedness Meta.NoSourceStrictness

-- | Generate a GADT definition based on a list of constructors
defaultDefinition :: [Meta.Con] -> Meta.Dec
defaultDefinition constructors =
  Meta.DataD context actionName typeVars kind constructors derivingClauses
 where
  context =
    []
  typeVars =
    [Meta.PlainTV $ Meta.mkName "a"]
  kind =
    Nothing
  derivingClauses =
    []
