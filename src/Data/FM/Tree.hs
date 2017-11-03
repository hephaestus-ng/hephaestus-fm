module Data.FM.Tree where

-- Module of Feature Tree representation and operations

import Control.Lens
-- import Control.Zipper
import Data.FM.Feature
import Data.Tree
import Data.Tree.Lens
import Data.Tree.Pretty

type FeatureTree = Tree Feature


-- COMBINATOR LIBRARY --
------------------------

-- Creates an OrFeature whose children are only leafs
-- ex.: (<+>) ("persistence", Mandatory) ["sql", "nosql"]
(<+>) :: (String, FeatureType) -> [String] -> FeatureTree
(n, t) <+> fs = Node (Feature n OrFeature t)
                     (map (\s -> Node (Feature s BasicFeature Optional) []) fs)



fm01 = Node (Feature "iris" BasicFeature Mandatory) [
         (Node (Feature "security" OrFeature Mandatory) [
            (Node (Feature "sha-256" BasicFeature Optional) []),
            (Node (Feature "RSA" BasicFeature Optional) [])
         ]),
         (Node (Feature "persist" AltFeature Mandatory) [
            (Node (Feature "SQL" BasicFeature Optional) []),
            (Node (Feature "NoSQL" BasicFeature Optional) [])
         ])
        ]


-- usar monad Maybe?
-- hasFeature :: String -> FeatureTree -> Bool
-- hasFeature f (Node t [])     = False
-- hasFeature f (Node t (x:xs)) =
--     if (f == (view name t)) then True
--     else map (\x -> hasFeature f x) (x:xs)
