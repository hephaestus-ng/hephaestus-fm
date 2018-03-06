module Parser.XML.Struct where

import Data.Tree
import Text.Parsec

import Data.FM.Tree
import Data.FM.Feature

import Parser.XML.Utils


parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree =
  string "<struct>"    >> blanks >>
  parseFeature >>= \ft -> blanks >>
  string "</struct>"   >> blanks >>
  return ft


parseFeature :: Parsec String () FeatureTree
parseFeature = andFeature <|>
               orFeature  <|>
               altFeature <|>
               childFeature


andFeature :: Parsec String () FeatureTree
andFeature =
  try (string "<and")           >> blanks >>
  parseFeatureData  >>= \(t, n) -> blanks >>
  many parseFeature >>= \cs     -> blanks >>
  string "</and>"               >> blanks >>
  return (Node (Feature n BasicFeature t) cs)


orFeature :: Parsec String () FeatureTree
orFeature =
  try (string "<or")            >> blanks >>
  parseFeatureData  >>= \(t, n) -> blanks >>
  many parseFeature >>= \cs     -> blanks >>
  string "</or>"                >> blanks >>
  return (Node (Feature n OrFeature t) cs)


altFeature :: Parsec String () FeatureTree
altFeature =
  try (string "<alt")           >> blanks >>
  parseFeatureData  >>= \(t, n) -> blanks >>
  many parseFeature >>= \cs     -> blanks >>
  string "</alt>"               >> blanks >>
  return (Node (Feature n AltFeature t) cs)


childFeature :: Parsec String () FeatureTree
childFeature =
  try (string "<feature")       >> blanks >>
  parseFeatureData  >>= \(t, n) -> blanks >>
  return (Node (Feature n BasicFeature t) [])


parseFeatureData :: Parsec String () (FeatureType, String)
parseFeatureData =
  parseFeatureType >>= \t -> blanks >>
  parseFeatureName >>= \n -> endTag >>
  return (t, n)


parseFeatureName :: Parsec String () String
parseFeatureName =
  string "name=\""  >> blanks >>
  many1 letter     >>= \n -> char '"' >>
  return n


parseFeatureType :: Parsec String () FeatureType
parseFeatureType =
  string "mandatory=\"" >> blanks >>
  many1 letter >>=
    \t -> char '"' >> blanks >>
      case t of
        "true"  -> return Mandatory
        "false" -> return Optional
