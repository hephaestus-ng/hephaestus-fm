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


parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree =
  parseFeature      >>= \root c -> blanks >>
  parseFeatureLeafs >>= \leafs  -> blanks >>
  --string "</and>"                                >> blanks >> 
  return (Node root leafs) 

 -- childs will be a feature list

parseFeatureLeafs :: Parsec String () FeatureTree
parseFeatureLeafs =
  parseFeature  >>= \f childs ->
    case childs of
      True  -> Node f [ parseFeatureLeafs ] 
      False -> Node f []


parseFeature :: Parsec String () Feature
parseFeature = 
  char '<'                 >> blanks >>
  parseFeatureGroup >>= \g -> blanks >>
  parseFeatureType  >>= \t -> blanks >>
  parseFeatureName  >>= \n -> blanks >>
  string >>= \childs -> 
    if childs == "/>" then
      return Feature n g t
    else
      return [Feature n g t, parseFeature]
  

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
  string "mandatory=\"" >> blanks
  many1 letter >>= 
    \t -> char '"' >> blanks >>
      case t of 
        "true"  -> Mandatory
        "false" -> Optional


