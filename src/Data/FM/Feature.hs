module Data.FM.Feature where

-- Module for defining representation of Feature structures

type Required = Bool


data FeatureGroup = BasicFeature | OrFeature | AlternativeFeature
 deriving(Show)


data FeatureType = Mandatory | Optional
 deriving(Show)
 

data Feature = Feature {
    name   :: String,
    group  :: FeatureGroup,
    typeF  :: FeatureType
} deriving(Show)



featureName :: Feature -> String
featureName (Feature name _ _) = name
