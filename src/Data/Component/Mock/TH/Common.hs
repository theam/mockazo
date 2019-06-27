module Data.Component.Mock.TH.Common
  ( actionName
  , titleizeName
  , functionTypeToList
  , makePrimeVars
  , makeVars
  , Field(..)
  , toField
  , toVarExp
  ) where

import Relude
import Data.Char

import Language.Haskell.TH as Meta
import Language.Haskell.TH.Syntax as Meta

-- | The default name for the action
actionName :: Meta.Name
actionName = Meta.mkName "Action"

-- | Converts a name to the equivalent titleized
--
-- >> titleizeName $ mkName "action"
-- Name "Action"
titleizeName :: Meta.Name -> Meta.Name
titleizeName name =
  Meta.nameBase name
  & capitalizeFirst
  & Meta.mkName
 where
  capitalizeFirst (c : cs) = toUpper c : cs
  capitalizeFirst other = other

-- | Converts a function type into a list of types, skipping the arrows
functionTypeToList :: Meta.Type -> [Meta.Type]
functionTypeToList type' =
  case type' of
    Meta.AppT (Meta.AppT Meta.ArrowT t1) t2 ->
      t1: functionTypeToList t2

    Meta.SigT t _ ->
      functionTypeToList t

    Meta.ForallT _ _ t ->
      functionTypeToList t

    t ->
      [t]

-- | Creates a list of n elements from
-- the sequence from a' to z'
makePrimeVars :: Int -> [Meta.Pat]
makePrimeVars n =
  ['a'..'z']
  & fmap one
  & fmap (<> "'")
  & take n
  & makeVarsWith

-- | Creates a list of n elements from
-- the sequence from a to z
makeVars :: Int -> [Meta.Pat]
makeVars n =
  ['a'..'z']
  & fmap one
  & take n
  & makeVarsWith

data Field = Field
  { name :: Meta.Name
  , argumentsLength :: Int
  }

toField :: Meta.VarBangType -> Field
toField (fieldName, _, fieldType) = do
  let functionTypes = functionTypeToList fieldType
  Field
    { name = fieldName
    , argumentsLength = length functionTypes - 1
    }

toVarExp :: Meta.Pat -> Meta.Exp
toVarExp (Meta.VarP varName) =
  Meta.VarE varName
toVarExp other =
  error
    $ "Mockazo: Error when running 'toVarExp' for value '" <> show other <> "'\n"
    <> "Please file an issue for this at https://github.com/theam/mockazo/issues"

makeVarsWith :: [String] -> [Meta.Pat]
makeVarsWith varNames =
  varNames
  & fmap makeVar
 where
  makeVar varName =
    Meta.VarP (Meta.mkName varName)
