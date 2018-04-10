module Data.FM.Utils where

import Control.Lens
import Data.Tree
import Data.FM.Types
import Data.Tree.Pretty


featureTreeToString :: FeatureTree -> Tree String
featureTreeToString (Node f [])     = Node (view name f) []
featureTreeToString (Node f (x:xs)) = Node (view name f)
                                       (map (\x -> featureTreeToString x) (x:xs))


printFeatureTree  t = putStrLn $ drawTree (featureTreeToString t)
pprintFeatureTree t = putStrLn $ drawVerticalTree (featureTreeToString t)
