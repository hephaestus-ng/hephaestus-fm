module Data.FM.Utils where

-- Module for helper functions to auxiliate reasoning of feature models

import Control.Lens
import Data.Tree
import Data.FM.Tree
import Data.FM.Feature
import Data.Tree.Pretty


featureTreeToString :: FeatureTree -> Tree String
featureTreeToString (Node f [])     = Node (view name f) []
featureTreeToString (Node f (x:xs)) = Node (view name f)
                                       (map (\x -> featureTreeToString x) (x:xs))


printFeatureTree  t = putStrLn $ drawTree (featureTreeToString t)
pprintFeatureTree t = putStrLn $ drawVerticalTree (featureTreeToString t)




-- unfoldTree function usage example
-- let t = unfoldTree (\i -> (show i, [i`div`2..i-1]))

-- usar monad Maybe?
-- hasFeature :: String -> FeatureTree -> Bool
-- hasFeature f (Node t [])     = False
-- hasFeature f (Node t (x:xs)) =
--     if (f == (view name t)) then True
--     else map (\x -> hasFeature f x) (x:xs)
