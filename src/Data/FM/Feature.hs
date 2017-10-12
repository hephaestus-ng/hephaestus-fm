module Data.FM.Feature where

-- Module for defining representation of Feature structures

type Required = Bool


data FeatureGroup = BasicFeature | OrFeature | AltFeature
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

featureGroup :: Feature -> FeatureGroup
featureGroup (Feature _ group _) = group

featureType :: Feature -> FeatureType
featureType (Feature _ _ ftype) = ftype
