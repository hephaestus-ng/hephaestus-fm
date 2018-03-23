module Lib where

import Data.FM.Feature
import Data.FM.Tree
import Data.FM.FeatureModel
import Data.FM.Expression
import Data.FM.SAT
import Data.FM.Utils

-- import Data.FM.Main

import Parser.XML.Main

interaction :: IO ()
interaction = do
  header
  options
  opt <- getLine
  case opt of
    "1" -> putStrLn "Implementing this feature"
    "2" -> loadXML
  deriveProduct


loadXML = do
  putStrLn "\n Specify the XML absolute path:"
  path <- getLine
  putStrLn "\n This is the Feature Tree description of the XML Feature Model: \n"
  fm <- parserXML path
  print fm



deriveProduct = do
  putStrLn "\n Specify the product derivation on a List with the feature names. i.e: ['iris', 'email', '...']"
  productDerivation <- getLine
  putStrLn productDerivation



--
-- Static interactions
--

header = do
  putStrLn "\n Welcome to Hephaestus-FM module, built to help you design, validate and derive products from specified Feature Models"
  putStrLn "\n First, choose an option of interaction: "


options = do
  putStrLn "1 - Create a Feature Model through command line"
  putStrLn "2 - Load a FeatureIDE XML through specified path"
