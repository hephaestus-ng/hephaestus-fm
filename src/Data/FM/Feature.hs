{-# LANGUAGE TemplateHaskell #-}

-- Module for defining representation of Feature structures
module Data.FM.Feature where

import Control.Lens


type Required = Bool

data FeatureGroup = BasicFeature | OrFeature | AltFeature
 deriving(Show)

data FeatureType = Mandatory | Optional
 deriving(Show)


data Feature = Feature {
    _name   :: String,
    _group  :: FeatureGroup,
    _typeF  :: FeatureType
} deriving(Show)
makeLenses ''Feature
