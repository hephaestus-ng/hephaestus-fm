module Parser.Parsec where

import Data.Tree
import Text.Parsec
import Text.Parsec.String

import Data.FM.FeatureModel
import Data.FM.Tree
import Data.FM.Feature
import Data.FM.Expression

parseFIDE :: Parsec String () FeatureModel
parseFIDE =
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
  string  "<and mandatory=\"true\" name=\"" >> blanks >> 
  many1 letter                              >>= \root -> blanks >>  
  string "\">"                              >> blanks >>
  string "</and>"                           >> blanks >> 
  return (Node (Feature root BasicFeature Mandatory) []) 

blanks = many (space <|> newline) 

-- parseFeatureType :: Parsec String () FeatureType
-- parseFeatureType =
--   many1 letter >>= \t ->
--   case t of
--     "and" -> return BasicType
--     "or"  -> return BasicType 
