{-# LANGUAGE TemplateHaskell #-}

module Data.FM.ProductConfiguration where

import Control.Lens
import Data.Tree
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.FeatureModel


data ProductConfiguration = ProductConfiguration {
  _featureModel      :: FeatureModel,
  _productDerivation :: [String],
  _valid             :: Bool
} deriving (Show, Eq)
makeLenses ''ProductConfiguration


prodConfig = ProductConfiguration fm02 ["iris", "security", "RSA", "persist"]


makeExprConstraints :: ProductConfiguration -> [FeatureExp]
makeExprConstraints pc =
  fmToFeatureExpr (view featureModel pc) ++
  prToFeatureExpressions (view productDerivation pc)




prToFeatureExpressions :: FeatureModel -> [String] -> [FeatureExp]
prToFeatureExpressions fm pr =
  do
    ftree <- (view featureTree fm)
    return makeExprConstraints ftree


-- | Generates a constraint feature expression list, where the Mandatory features
--   are negated at first moment, and validated through the users product configuration
makeExprConstraints :: FeatureTree -> [FeatureExp]
makeExprConstraints (Node f [])     = []
makeExprConstraints (Node f (x:xs)) =
  checkTypeRules f fs      ++
  checkGroupRules f (x:xs) ++
  (map (\x -> featureTreeToString x) (x:xs))


checkTypeRules :: Feature -> [FeatureExp]
checkTypeRules f fs =
  case (view typeF f) of
    Mandatory -> Not $ Ref (view name f)
    Optional  -> []


-- checkGroupRules :: Feature -> FeatureTree -> [Feature] -> FeatureExp
-- checkGroupRules f fs =
--   case (view group f) of


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
