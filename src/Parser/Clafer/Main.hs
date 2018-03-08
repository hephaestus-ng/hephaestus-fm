module Parser.Clafer.Main where

import Control.Lens
import Data.Tree
import Data.FM.Tree
import Data.FM.Feature
import Data.FM.Expression
import Data.FM.FeatureModel
import Data.FM.Utils

import Text.Parsec
import Text.Parsec.String

path = "/home/thi4go/Haskell/hephaestus-fm/src/Parser/Clafer/model-example.clafer"


main = parseFromFile parseClaferModel path >>= \result ->
  case result of
    Left err -> print err
    Right fm -> (pprintFeatureTree (view featureTree fm))


blanks = many (space <|> newline)


parseClaferModel :: Parsec String () FeatureModel
parseClaferModel =
  parseFeatureTree >>= \ft -> blanks >>
  return (FeatureModel ft [])


parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree =
  many1 letter >>= \root -> blanks >>
  parseFeature >>= \ft   -> blanks >>
  return (Node (Feature root BasicFeature Mandatory) [ft])


parseFeature :: Parsec String () FeatureTree
parseFeature =
  altFeature       <|>
  childFeatureType <|>
  childFeature


altFeature :: Parsec String () FeatureTree
altFeature =
  try (string "xor")        >> blanks >>
  many1 letter      >>= \n  -> blanks >>
  many parseFeature >>= \cs -> blanks >>
  return (Node (Feature n AltFeature Mandatory) cs)


childFeature :: Parsec String () FeatureTree
childFeature =
  many1 letter >>= \n -> blanks >>
  return (Node (Feature n BasicFeature Mandatory) [])

childFeatureType :: Parsec String () FeatureTree
childFeatureType =
  many1 letter >>= \n -> space >>
  try (char '?')      >> blanks >>
  return (Node (Feature n BasicFeature Optional) [])



-- isMandatory = try (char '?') <|>
