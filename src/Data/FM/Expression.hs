module Data.FM.Expression where

-- Module to reason on feature model derivations in propositional logic expressions

import Control.Lens
import Data.Tree
import Data.Tree.Lens
import Data.FM.Tree
import Data.FM.Feature


data FeatureExp = B Bool
                | Ref String
                | And FeatureExp FeatureExp
                | Or FeatureExp FeatureExp
                | Not FeatureExp
                -- | Tuple (FeatureExp, [FeatureExp])
    deriving(Show, Eq)


-- eval :: FeatureExp -> ProductConfiguration -> Bool
-- eval (B b) _            = b
-- eval (Ref s) pc         = s `elem` pc
-- eval (And exp1 exp2) pc = (eval exp1 pc) && (eval exp2 pc)
-- eval (Or exp1 exp2) pc  = (eval exp1 pc) || (eval exp2 pc)
-- eval (Not exp1) pc      = not (eval exp1 pc)



featureTreeToExp :: FeatureTree -> [FeatureExp]
featureTreeToExp (Node f [])     = []
featureTreeToExp (Node f (x:xs)) = (featureTreeToExp' f (x:xs)) ++ concatMap featureTreeToExp (x:xs)
    where
      featureTreeToExp' f (x:xs) = case (view group f) of
        BasicFeature -> map (convert f) (map (\(Node f _) -> f) (x:xs))

        OrFeature    -> (Ref (view name f) .=> foldr Or (B False) (map (\(Node f _) -> Ref (view name f)) (x:xs)))
                        : [(Ref (view name c)) .=> Ref (view name f) | (Node c _) <- (x:xs)]

        AltFeature   -> (Ref (view name f) .=> foldr xor (B False) (map (\(Node f _) -> Ref (view name f)) (x:xs)))
                        : [(Ref (view name c)) .=> Ref (view name f) | (Node c _) <- (x:xs)]


convert parent child =
    case (view typeF child) of
      Mandatory -> Ref (view name parent) <=> Ref (view name child)
      Optional  -> Ref (view name child) .=> Ref (view name parent)



(.=>) :: FeatureExp -> FeatureExp -> FeatureExp
p .=> q = Or (Not p) q

(<=>) :: FeatureExp -> FeatureExp -> FeatureExp
p <=> q = And (Or (Not p) q) (Or p (Not q))

xor :: FeatureExp -> FeatureExp -> FeatureExp
p `xor` q = (p \/ q) /\ (Not (p /\ q))

(/\) :: FeatureExp -> FeatureExp -> FeatureExp
p /\ q = And p q

(\/) :: FeatureExp -> FeatureExp -> FeatureExp
p \/ q = Or p q


choose :: [FeatureExp] -> Int -> FeatureExp
choose fs n = fs !! n

chooseMany :: [FeatureExp] -> Int -> FeatureExp
chooseMany fs 0 = B True
chooseMany fs n = And (choose fs n) (chooseMany fs (n-1))
