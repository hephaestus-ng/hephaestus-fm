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


main = parseFromFile parseFeatureIDE "fm.ide" >>= \result ->
  case result of
    Left err -> print err
    Right fm -> print (show fm)

blanks = many (space <|> newline)


parseFeatureIDE :: Parsec String () FeatureModel
parseFeatureIDE =
  parseHeader      >> blanks >>
  parseFeatureTree >>= \ft -> blanks >>
  parseConstraints >>= \cs -> blanks >>
  parseFooter      >> blanks >>
  return (FeatureModel ft [])


parseConstraints :: Parsec String () [FeatureExp]
parseConstraints =
  string "<constraints>" >> blanks >>
  parseRules >>= \rs -> blanks >>
  string "</constraints" >> blanks >>
  return [rs]


parseRules :: Parsec String () FeatureExp
parseRules =
  string "<rule>"                             >> blanks >>
  string "<eq>"                               >> blanks >>
  --many1 letter   >>= \r -> char '>'           >> blanks >>
  string "<var>"                              >> blanks >>
  many1 letter   >>= \ref1 -> string "</var>" >> blanks >>
  string "<var>"                              >> blanks >>
  many1 letter   >>= \ref2 -> string "</var>" >> blanks >>
  string "</eq>"                              >> blanks >>
  string "</rule>"                            >> blanks >>
  return (Ref ref1 <=> Ref ref2)
    -- case r of
    --   "eq"  -> return (Ref ref1 <=> Ref ref2)
    --   "imp" -> return (Ref ref1 .=> Ref ref2)



--parseFeatureTree :: Parsec String () FeatureTree
--parseFeatureTree =
--  parseFeature      >>= \(root, cs) -> blanks >>
  -- parseFeatureLeafs >>= \leafs      -> blanks >>
  -- return (Node root leafs)

--parseFeatureLeafs :: Parsec String () [Tree Feature]
--parseFeatureLeafs =
--  parseFeature  >>= \(f, cs) ->
--    case cs of
--      True  -> return Node f [ parseFeatureLeafs ]
--      False -> return Node f []

-- parseFeatureTree :: Parsec String () FeatureTree
-- parseFeatureTree = parseEnv [] parseFeature
--   where
--     parseEnv env

parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree =
  string "<struct>"   >> blanks >>
  parseFeature >>= \f -> blanks >>
  string "</struct>"  >> blanks >>

  return (Node f [])


parseFeature :: Parsec String () Feature
parseFeature =
  char '<'                 >> blanks >>
  parseFeatureGroup >>= \g -> blanks >>
  parseFeatureType  >>= \t -> blanks >>
  parseFeatureName  >>= \n -> blanks >>
  char '>'                 >> blanks >>
  string "</and>"          >> blanks >>
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


parseHeader :: Parsec String () ()
parseHeader =
  string "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" >> blanks >>
  string "<featureModel chosenLayoutAlgorithm=\"1\">"                   >> blanks >>
  return ()


parseFooter :: Parsec String () ()
parseFooter =
  string "<calculations Auto=\"true\" Constraints=\"true\" Features=\"true\" Redundant=\"true\"/>" >> blanks >>
  string "<comments/>" >> blanks >>
  string "<featureOrder userDefined=\"false\"/>" >> blanks >>
  string "</featureModel>" >> blanks >>
  return ()
