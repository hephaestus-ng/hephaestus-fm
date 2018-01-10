{-# LANGUAGE TemplateHaskell #-}

module Data.FM.FeatureModel where

import Control.Lens
import Data.Tree
import Data.Tree.Lens
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.Expression


data FeatureModel = FeatureModel {
    _featureTree :: FeatureTree,
    _expressions :: [FeatureExp]
} deriving (Show, Eq)
makeLenses ''FeatureModel


-- Generates propositional logic expressions for a given Feature Model
fmToFeatureExpr :: FeatureModel -> [FeatureExp]
fmToFeatureExpr fm =
    featureTreeToExp (view featureTree fm) ++
    (view expressions fm) ++
    [Ref (view name $ view root $ view featureTree fm)]
