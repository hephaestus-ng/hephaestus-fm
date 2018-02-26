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


xmlFMDef :: LanguageDef st
xmlFMDef = LanguageDef
          { commentStart    = ""
          , commentEnd      = ""
          , commentLine     = ""
          , nestedComments  = False
          , identStart      = letter <|> char '_'
          , identLetter     = alphaNum <|> oneOf "_'"
          , opStart         = opLetter emptyDef
          , opLetter        = oneOf ""
          , reservedOpNames = []
          , reservedNames   = ["<featureModel", "<struct>", 
                               "<and", "<alt", "<or",
                               "<feature", "<constraints>",
                               "<rule>", "<var>", "<eq>", 
                               "<imp>"]
          , caseSensitive   = True
          }


lexer = makeTokenParser xmlFMDef

parseReservedNames = reserved lexer
parseString        = stringLiteral lexer

main = parseFromFile parseFIDE "fm.ide" >>= \result ->
       case result of
         Left err -> print err
         Right fm -> print (show fm) 
  

         
parseFIDE :: Parsec String () String
parseFIDE =
  do
    s      <- parseString
    result <- parseReservedNames s
    return (s)         
{- 
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
-}

parseFeatureTree :: Parsec String () FeatureTree
parseFeatureTree =
  string  "<and mandatory=\"true\" name=\"" >> blanks >> 
  many1 letter                              >>= \root -> blanks >>  
  string "\">"                              >> blanks >>
  string "</and>"                           >> blanks >> 
  return (Node (Feature root BasicFeature Mandatory) []) 

blanks = many (space <|> newline) 


parseFromFileLocal p fname = 
  do
    input <- readFile fname
    return (runParser p () fname input)
-- parseFeatureType :: Parsec String () FeatureType
-- parseFeatureType =
--   many1 letter >>= \t ->
--   case t of
--     "and" -> return BasicType
--     "or"  -> return BasicType 
