module Parser.XML.Main where

import Control.Lens

import Text.Parsec
import Text.Parsec.String

import Data.FM.FeatureModel
import Data.FM.Utils

import Parser.XML.Struct
import Parser.XML.Constraints
import Parser.XML.Utils


path = "/home/thi4go/Haskell/hephaestus-fm/test/Test/Parser/fm.ide"


main = parseFromFile parseFeatureIDE path >>= \result ->
  case result of
    Left err -> print err
    Right fm -> (pprintFeatureTree (view featureTree fm))


parseFeatureIDE :: Parsec String () FeatureModel
parseFeatureIDE =
  parseHeader              >> blanks >>
  parseFeatureTree >>= \ft -> blanks >>
  parseConstraints >>= \cs -> blanks >>
  parseFooter              >> blanks >>
  return (FeatureModel ft cs)



--
-- Parse unused XML markups
--

parseHeader :: Parsec String () ()
parseHeader =
  string "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" >> blanks >>
  string "<featureModel chosenLayoutAlgorithm=\"1\">"                   >> blanks >>
  return ()


parseFooter :: Parsec String () ()
parseFooter =
  string "<calculations Auto=\"true\" Constraints=\"true\" Features=\"true\" Redundant=\"true\"/>" >> blanks >>
  string "<comments/>"                           >> blanks >>
  string "<featureOrder userDefined=\"false\"/>" >> blanks >>
  string "</featureModel>"                       >> blanks >>
  return ()
