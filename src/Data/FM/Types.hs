{-# LANGUAGE TemplateHaskell #-}

module Data.FM.Types where

import Control.Lens
import Data.Tree


type Required = Bool

type FeatureTree = Tree Feature

type ProductConfiguration = [String]


data FeatureGroup = BasicFeature | OrFeature | AltFeature
  deriving(Show, Eq)

data FeatureType = Mandatory | Optional
  deriving(Show, Eq)

data Feature = Feature {
    _name   :: String,
    _group  :: FeatureGroup,
    _typeF  :: FeatureType
} deriving(Show, Eq)
makeLenses ''Feature

data FeatureExp = B Bool
                | Ref String
                | And FeatureExp FeatureExp
                | Or FeatureExp FeatureExp
                | Not FeatureExp
  deriving(Show, Eq)

data FeatureModel = FeatureModel {
    _featureTree :: FeatureTree,
    _expressions :: [FeatureExp]
} deriving (Show, Eq)
makeLenses ''FeatureModel
