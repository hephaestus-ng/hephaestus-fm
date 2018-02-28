{-# LANGUAGE FlexibleContexts #-}

module Parser.Parsec where

import Data.Tree
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

import Data.FM.FeatureModel
import Data.FM.Tree
import Data.FM.Feature
import Data.FM.Expression


blanks = many (space <|> newline) 

parseFeatureIDE :: Parsec String () FeatureModel
parseFeatureIDE =
  string "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" >> blanks >> 
  string "<featureModel chosenLayoutAlgorithm=\"1\">"                   >> blanks >>
  string "<struct>"                                                     >> blanks >> 
  parseFeatureTree                                                      >>= \ft -> blanks >> 
  string "</struct>"                                                    >> blanks >> 
  string "<calculations Auto=\"true\" Constraints=\"true\" Features=\"true\" Redundant=\"true\"/>" >> blanks >>
  string "<comments/>" >> blanks >> 
  string "<featureOrder userDefined=\"false\"/>" >> blanks >>  
  string "</featureModel>" >> blanks >> 
  return (FeatureModel ft [])


--parseFeatureTree :: Parsec String () FeatureTree
--parseFeatureTree =
--  parseFeature      >>= \(root, cs) -> blanks >>
  -- parseFeatureLeafs >>= \leafs      -> blanks >>
  -- return (Node root leafs) 

 -- childs will be a feature list


--parseFeatureLeafs :: Parsec String () [Tree Feature]
--parseFeatureLeafs =
--  parseFeature  >>= \(f, cs) ->
--    case cs of
--      True  -> return Node f [ parseFeatureLeafs ] 
--      False -> return Node f []

parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree = undefined

parseFeature :: Parsec String () Feature
parseFeature = 
  char '<'                 >> blanks >>
  parseFeatureGroup >>= \g -> blanks >>
  parseFeatureType  >>= \t -> blanks >>
  parseFeatureName  >>= \n -> blanks >>
  char '>'                 >> blanks >>
  return (Feature n g t)
  

parseFeatureName :: Parsec String () String
parseFeatureName =
  string "name=\""  >> blanks >>
  many1 letter     >>= \n -> char '"' >>
  return n 


parseFeatureGroup :: Parsec String () FeatureGroup
parseFeatureGroup =
  many1 letter >>= 
    \g ->
      case g of
        "and"     -> return BasicFeature
        "or"      -> return OrFeature
        "alt"     -> return AltFeature
        "feature" -> return BasicFeature


parseFeatureType :: Parsec String () FeatureType 
parseFeatureType = 
  string "mandatory=\"" >> blanks >>
  many1 letter >>= 
    \t -> char '"' >> blanks >>
      case t of 
        "true"  -> return Mandatory
        "false" -> return Optional


