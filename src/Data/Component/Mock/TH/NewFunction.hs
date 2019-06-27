module Data.Component.Mock.TH.NewFunction
  (make) where

import Relude

import qualified Language.Haskell.TH as Meta
import qualified Language.Haskell.TH.Syntax as Meta

import Data.Component.Mock.TH.Common

make :: Meta.Name -> [Meta.VarBangType] -> Meta.DecsQ
make recordName fields = pure $
  [ Meta.SigD
      mockName
      (Meta.ForallT
        []
        [ Meta.AppT
            (Meta.AppT
              (Meta.ConT $ Meta.mkName "Executes")
              (Meta.ConT $ Meta.mkName "Action"))
            (Meta.VarT $ Meta.mkName "actions")
        ]
        (Meta.AppT
          (Meta.ConT recordName)
          (Meta.AppT
            (Meta.ConT $ Meta.mkName "InContextOf")
            (Meta.VarT $ Meta.mkName "actions"))))
  , Meta.ValD
     (Meta.VarP mockName)
     (Meta.NormalB
       (Meta.RecConE
         (eraseNameInformation recordName)
         (fmap (mockField . toField) fields)
         )) []
  ]

mockField :: Field -> (Meta.Name, Meta.Exp)
mockField Field{..} = do
  let varExps = fmap toVarExp $ makeVars argumentsLength
  let actionConstructor = Meta.ConE (titleizeName name)
  let appliedConstructor = foldl' Meta.AppE actionConstructor varExps
  ( name
    , Meta.LamE
      (makeVars argumentsLength)
      (Meta.AppE
        (Meta.AppE
          (Meta.VarE $ Meta.mkName "mockAction")
          (Meta.LitE $ Meta.StringL $ Meta.nameBase name))
        appliedConstructor)
    )

mockName :: Meta.Name
mockName = Meta.mkName "mock"

eraseNameInformation :: Meta.Name -> Meta.Name
eraseNameInformation =
  Meta.mkName . Meta.nameBase
