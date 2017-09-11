module Data.FM.Utils where

-- Module for helper functions to auxiliate reasoning of feature models

import Data.FM.Tree
import Data.FM.Feature
import Data.Tree

type StringTree = Tree String

printFMtree :: FeatureTree -> [String]
printFMtree ft = map (featureName) (flatten ft)

transformFeatureTree :: FeatureTree -> StringTree
transformFeatureTree (Node a []) = Node (featureName a) []
transformFeatureTree (Node a f)  = Node (featureName a) (transformFeatureTree f)


-- showFeatureTree t = putStr . drawTree t

-- transformFeatureTree :: FeatureTree -> FeatureTree



--let fm01 = Node (Feature "iris" OrFeature Mandatory) [(Node (Feature "security" OrFeature Mandatory) []), (Node (Feature "persist" OrFeature Mandatory) [])]



-- unfoldTree function usage example
-- let t = unfoldTree (\i -> (show i, [i`div`2..i-1]))
