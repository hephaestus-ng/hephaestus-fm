module Parser.XML.Constraints where

import Text.Parsec
import Text.Parsec.String

import Data.FM.Expression

import Parser.XML.Utils

parseConstraints :: Parsec String () [FeatureExp]
parseConstraints =
  string "<constraints>" >> blanks >>
  many (try parseRules) >>= \rs -> string "</constraints>" >> blanks >>
  return rs


parseRules :: Parsec String () FeatureExp
parseRules =
  string "<rule>"                             >> blanks >>
  char '<'                                    >> blanks >>
  many1 letter   >>= \r -> char '>'           >> blanks >>
  string "<var>"                              >> blanks >>
  many1 letter   >>= \ref1 -> string "</var>" >> blanks >>
  string "<var>"                              >> blanks >>
  many1 letter   >>= \ref2 -> string "</var>" >> blanks >>
  string ("</" ++ r ++ ">")                   >> blanks >>
  string "</rule>"                            >> blanks >>
    case r of
      "eq"  -> return (Ref ref1 <=> Ref ref2)
      "imp" -> return (Ref ref1 .=> Ref ref2)
