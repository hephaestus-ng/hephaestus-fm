module Main where

import Text.Parsec 
import Text.Parsec.String

import Parser.Parsec 

main = parseFromFile parseFIDE "fm.ide" >>= \result ->
       case result of
         Left err -> print err
         Right fm -> print (show fm) 
  
