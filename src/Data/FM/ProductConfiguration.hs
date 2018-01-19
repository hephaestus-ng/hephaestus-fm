module Data.FM.ProductConfiguration(ProductConfiguration, isValid) where

import Control.Lens
import Data.Tree
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.FeatureModel


type ProductConfiguration = [String]

isValid :: FeatureModel -> ProductConfiguration -> Bool
isValid fm pc = all (`isValidExp` pc) exps
  where exps = fmToFeatureExpr fm

isValidExp :: FeatureExp -> ProductConfiguration -> Bool
isValidExp (B b) pc        = b
isValidExp (Ref ref) pc    = ref `elem` pc
isValidExp (Not expr) pc   = not (isValidExp expr pc)
isValidExp (And e1 e2) pc  = isValidExp e1 pc && isValidExp e2 pc
isValidExp (Or e1 e2) pc   = isValidExp e1 pc || isValidExp e2 pc
