module Data.FM.Utils where

-- Module for helper functions to auxiliate reasoning of feature models

-- import Data.Tree.Pretty

import Data.FM.Tree
import Data.FM.Feature
import Data.Tree
-- import Data.Maybe

type StringTree = Tree String

printFMtree :: FeatureTree -> [String]
printFMtree ft = map (featureName) (flatten ft)


-- buildFeatureNameTree :: [Feature] -> StringTree
-- buildFeatureNameTree fs = case fs of
--     []     -> Node "" []
--     (f:fs) -> Node (featureName f) (buildFeatureNameTree fs)


-- transformFeatureTree :: FeatureTree -> Tree String
-- transformFeatureTree (Node a []) = Node (featureName a) []
-- transformFeatureTree (Node a f)  = Node (featureName a) (transformFeatureTree f)
-- transformFeatureTree f = case f of
--     (Node s []) -> Node (featureName s) []
--     (Node s sf) -> Node (featureName s) (transformFeatureTree f:sf)


-- showFeatureTree t = putStrLn $ drawTree t




-- let fm01 = Node (Feature "iris" OrFeature Mandatory) [(Node (Feature "security" OrFeature Mandatory) []), (Node (Feature "persist" OrFeature Mandatory) [])]



-- unfoldTree function usage example
-- let t = unfoldTree (\i -> (show i, [i`div`2..i-1]))
