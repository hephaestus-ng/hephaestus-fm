module Data.FM.Tree where

-- Module of Feature Tree representation and operations

import Control.Lens

import Data.FM.Feature
import Data.Tree
import Data.Tree.Lens

type FeatureTree = Tree Feature


-- Creates an OrFeature whose children are only leafs
-- ex.: (<+>) ("persistence", Mandatory) ["sql", "nosql"]
(<+>) :: (String, FeatureType) -> [String] -> FeatureTree
(n, t) <+> fs = Node (Feature n OrFeature t)
                     (map (\s -> Node (Feature s BasicFeature Optional) []) fs)



-- usar monad Maybe?
hasFeature :: String -> FeatureTree -> Bool
hasFeature f (Node t []) = False
hasFeature f (Node t ts) =
    if f == t.name then True
    else map (\b -> hasFeature f b) (view branches ts)


-- Tree Representation example
-- let fm01 = Node (Feature "iris" OrFeature Mandatory) [(Node (Feature "security" OrFeature Mandatory) []), (Node (Feature "persist" OrFeature Mandatory) [])]
