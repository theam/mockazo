module Data.Component.Mock.TH.Instance
  ( make
  ) where

import Relude

import qualified Language.Haskell.TH as Meta
import qualified Language.Haskell.TH.Syntax as Meta

import Data.Component.Mock.TH.Common

{-| Generates an instance for IsAction based on a record.

For a record like

@
data Component context = Component
  { rootDir          :: ProjectName -> context (Path Rel Dir)
  , getProjectConfig :: ProjectName -> context ProjectConfig
  , initProject      :: Path Rel Dir -> ProjectConfig -> context ()
  }
@

It will generate an instance like
@
instance IsAction Action where
  eqAction (RootDir a) (RootDir b) =
    if a == b then Just Refl else Nothing

  eqAction (GetProjectConfig a) (GetProjectConfig b) =
    if a == b then Just Refl else Nothing

  eqAction (InitProject a a') (InitProject b b') =
    if a == b && a' == b' then Just Refl else Nothing

  eqAction _ _ =
    Nothing
@
-}
make :: [Meta.VarBangType] -> Meta.DecQ
make rawFields = do
  let fields = fmap toField rawFields
  pure $ Meta.InstanceD
    Nothing
    []
    (Meta.AppT (Meta.ConT $ Meta.mkName "IsAction") (Meta.ConT actionName))
    (functionDeclarations fields)

functionDeclarations :: [Field] -> [Meta.Dec]
functionDeclarations fields =
  [ Meta.FunD
      (Meta.mkName "eqAction")
      (fmap makeClause fields <> [wildcardClause])
    ]

makeClause :: Field -> Meta.Clause
makeClause Field{..} =
  Meta.Clause
    [ Meta.ConP (titleizeName name) (makeVars argumentsLength)
    , Meta.ConP (titleizeName name) (makePrimeVars argumentsLength)
    ]
    (Meta.NormalB
      (Meta.CondE
        (makeCondition argumentsLength)
        (Meta.AppE
          (Meta.ConE $ Meta.mkName "Just")
          (Meta.ConE $ Meta.mkName "Refl"))
        (Meta.ConE $ Meta.mkName "Nothing")))
    []

wildcardClause :: Meta.Clause
wildcardClause =
  Meta.Clause
    [ Meta.WildP
    , Meta.WildP
    ]
    (Meta.NormalB
      (Meta.ConE $ Meta.mkName "Nothing"))
    []

makeCondition :: Int -> Meta.Exp
makeCondition varNumber = do
  let vars = fmap toVarExp $ makeVars varNumber
  let primeVars = fmap toVarExp $ makePrimeVars varNumber
  let comparedVars = zipWith compareVars vars primeVars
  case comparedVars of
    [condition] ->
      condition
    (condition : moreConditions) ->
      foldl' joinConditions condition moreConditions
    other ->
      error
      $ "Mockazo: Error when running 'makeCondition' for value '" <> show other <> "'\n"
      <> "Please file an issue for this at https://github.com/theam/mockazo/issues"
 where
  compareVars v v' =
    Meta.UInfixE v (Meta.VarE (Meta.mkName "<=>")) v'
  joinConditions c c' =
    Meta.UInfixE c (Meta.VarE (Meta.mkName "&&")) c'
