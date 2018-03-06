module Test.Parser.XMLTest where

import Control.Lens
import Test.HUnit
import Text.Parsec
import Text.Parsec.String

import Data.Tree
import Data.FM.Tree
import Data.FM.Utils
import Data.FM.Feature
import Data.FM.Expression
import Data.FM.FeatureModel

import Parser.XML.Main


path1 = "./fm.ide"
path2 = "./fm2.ide"

parserTest p = parseFromFile parseFeatureIDE p >>= \result ->
  case result of
    Left err -> print err
    Right fm -> print fm

-- parseFromFile

parserTest2 p fpath = do {
         input  <- readFile fpath;
         result <- (runParser p () fpath input);
         case result of
           Left err -> err
           Right fm -> fm
       }

fm1tree = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "expressionProblem" OrFeature Mandatory) [
                (Node (Feature "sendMessage" BasicFeature Mandatory) []),
                (Node (Feature "receiveMessage" BasicFeature Mandatory) [])
             ]),
             (Node (Feature "addressBook" BasicFeature Optional) []),
             (Node (Feature "persistence" AltFeature Mandatory) [
                (Node (Feature "relational" BasicFeature Mandatory) []),
                (Node (Feature "nonRelational" BasicFeature Mandatory) [])
             ])
            ]

fm1constraints = [ Ref "sign" <=> Ref "verify",
                   Ref "encrypt" <=> Ref "decrypt",
                   Ref "advancedSearch" .=> Ref "nonRelational",
                   Ref "simpleSearch" .=> Ref "relational"
                 ]

fm1 = FeatureModel fm1tree fm1constraints

test01 = TestCase (assertEqual "Feature Model Tree verification for fm.ide"
                  fm1 (parserTest2 parseFeatureIDE path1))

parserTests = TestList [ TestLabel "Parser testing for fm.ide example" test01]

runXMLParserTests = runTestTT parserTests
