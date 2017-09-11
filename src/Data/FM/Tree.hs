module Data.FM.Tree where

-- Module of Feature Tree representation and operations

import Data.FM.Feature
import Data.Tree


type FeatureTree = Tree Feature


-- Creates an OrFeature whose children are only leafs
-- ex.: (<+>) ("persistence", Mandatory) ["sql", "nosql"]
(<+>) :: (String, FeatureType) -> [String] -> FeatureTree
(n, t) <+> fs = Node (Feature n OrFeature t)
                     (map (\s -> Node (Feature s BasicFeature Optional) []) fs)


-- (<+>) :: Doc -> Doc -> Doc
-- (<+>) :: Feature -> FeatureList -> FeatureTree


-- Tree Representation example
--let fm01 = Node (Feature "iris" OrFeature Mandatory) [(Node (Feature "security" OrFeature Mandatory) []), (Node (Feature "persist" OrFeature Mandatory) [])]
