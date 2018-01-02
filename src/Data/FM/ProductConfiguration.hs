{-# LANGUAGE TemplateHaskell #-}

module Data.FM.ProductConfiguration where

import Control.Lens
import Data.Tree
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.FeatureModel


-- data Product = Product {
--   _productList :: [Feature],
--   _productTree :: FeatureTree
-- } deriving (Show, Eq)
-- makeLenses ''Product


data ProductConfiguration = ProductConfiguration {
  _featureModel      :: FeatureModel,
  _productDerivation :: [Feature],
  _valid             :: Bool
} deriving (Show, Eq)
makeLenses ''ProductConfiguration


prod       = Product derivTree (flatten derivTree)

prodConfig = ProductConfiguration fm02 ["iris", "security", "RSA", "persist"]


ruleChecker :: ProductConfiguration -> [FeatureExp]
ruleChecker pc = view featureTree $ view featureModel pc >>= \fmtree ->
                 view productf pc                        >>= \ps ->
                 getProductExpr fmtree pc


getProductExpr :: FeatureTree -> [Feature] -> [FeatureExp]
getProductExpr (Node f []) fs     = []

getProductExpr (Node f (x:xs)) fs =
  (checkTypeRules f fs) ++
  (checkGroupRules f (x:xs) fs) ++
  (map (\x -> getProductExpr x) (x:xs))



checkTypeRules :: Feature -> [Feature] -> FeatureExp
checkTypeRules f fs =
  case (view typeF f) of
    Mandatory -> if n@(view name f) `elem` fs then (Ref n) else Not (Ref n)
    Optional  ->


checkGroupRules :: Feature -> [Feature] -> FeatureExp
checkGroupRules f fs =
  case (view group f) of






-- (map (\x -> getProductExpr x) (x:xs))
ft02 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]

fm02 = FeatureModel ft02 []

derivTree = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [])
            ]
