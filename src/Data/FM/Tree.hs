module Data.FM.Tree where

-- Module of Feature Tree representation and operations

import Control.Lens
import Data.FM.Feature
import Data.Tree
import Data.Tree.Lens
import Data.Tree.Pretty
import Data.Tree.Zipper

type FeatureTree = Tree Feature


-- COMBINATOR LIBRARY --
------------------------

-- Creates an OrFeature whose children are only leafs
-- ex.: (<+>) ("persistence", Mandatory) ["sql", "nosql"]
(<+>) :: (String, FeatureType) -> [String] -> FeatureTree
(n, t) <+> fs = Node (Feature n OrFeature t)
                     (map (\s -> Node (Feature s BasicFeature Optional) []) fs)



featureTreeToString :: FeatureTree -> Tree String
featureTreeToString (Node f [])     = Node (view name f) []
featureTreeToString (Node f (x:xs)) = Node (view name f)
                                    (map (\x -> featureTreeToString x) (x:xs))


printFeatureTree  t = putStrLn $ drawTree (featureTreeToString t)
pprintFeatureTree t = putStrLn $ drawVerticalTree (featureTreeToString t)

-- Tree for testing porpuse
ft02 = Node (Feature "iris" BasicFeature Mandatory) [
         (Node (Feature "security" OrFeature Mandatory) [
            (Node (Feature "sha-256" BasicFeature Mandatory) []),
            (Node (Feature "RSA" BasicFeature Mandatory) [])
         ]),
         (Node (Feature "persist" AltFeature Mandatory) [
            (Node (Feature "SQL" AltFeature Mandatory) [
              (Node (Feature "PostgreSQL" BasicFeature Optional) []),
              (Node (Feature "MySQL" BasicFeature Optional) []),
              (Node (Feature "LiteSQL" BasicFeature Optional) [])
            ]),
            (Node (Feature "NoSQL" BasicFeature Mandatory) [
              (Node (Feature "MongoDB" BasicFeature Mandatory) [])
            ])
         ])
        ]
