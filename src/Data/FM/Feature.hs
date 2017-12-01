{-# LANGUAGE TemplateHaskell #-}

module Data.FM.Feature where

-- Module for defining representation of Feature structures

import Control.Lens


type Required = Bool

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
