{-# LANGUAGE TemplateHaskell #-}

module Data.FM.ProductConfiguration where

import Control.Lens
import Data.Tree
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.FeatureModel


data Product = Product {
    _productFTree :: FeatureTree,
    _productFList :: [Feature]
} deriving (Show, Eq)
makeLenses ''Product

data ProductConfiguration = ProductConfiguration {
    _featureModel   :: FeatureModel,
    _product        :: Product,
    _valid          :: Bool
} deriving (Show, Eq)
makeLenses ''ProductConfiguration


prod = Product derivTree (flatten derivTree)

eval :: ProductConfiguration -> Bool
eval _ = False

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
