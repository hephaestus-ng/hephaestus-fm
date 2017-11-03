module Data.FM.Expression where

-- Module to reason on feature model derivations in propositional logic expressions

import Test.HUnit

import Data.FM.Tree
import Data.FM.Feature
import Data.Tree

data FeatureExp = B Bool
                | Ref String
                | And FeatureExp FeatureExp
                | Or FeatureExp FeatureExp
                | Not FeatureExp
                -- | Tuple (FeatureExp, [FeatureExp])
    deriving(Show)

type ProductConfiguration = [String]

eval :: FeatureExp -> ProductConfiguration -> Bool
eval (B b) _            = b
eval (Ref s) pc         = s `elem` pc
eval (And exp1 exp2) pc = (eval exp1 pc) && (eval exp2 pc)
eval (Or exp1 exp2) pc  = (eval exp1 pc) || (eval exp2 pc)
eval (Not exp1) pc      = not (eval exp1 pc)


-- todo: convert feature tree to a propositional logic expression list, for validation
featureTreeToExp :: FeatureTree -> [FeatureExp]
featureTreeToExp (Node f (x:xs)) = case (group f) of

    BasicFeature -> map (convert f) (map (\(Node f _) -> f) (x:xs))

    OrFeature    -> (Ref (name f) .=> foldr Or (B False) (map (\(Node f _) -> Ref (name f))(x:xs)))
                    : [(Ref (name c)) .=> Ref (name f) | (Node c _) <- (x:xs)]

    AltFeature   -> (Ref (name f) .=> foldr xor (B False) (map (\(Node f _) -> Ref (name f))(x:xs)))
                    : [(Ref (name c)) .=> Ref (name f) | (Node c _) <- (x:xs)]


convert feature child =
    case (view typeF child) of
        Mandatory -> Ref (name feature) <=> Ref (name child)
        Optional  -> Ref (name child) .=> Ref (name feature)


(.=>) :: FeatureExp -> FeatureExp -> FeatureExp
p .=> q = Or (Not p) q

(<=>) :: FeatureExp -> FeatureExp -> FeatureExp
p <=> q = And (Or (Not p) q) (Or p (Not q))

(/\) :: FeatureExp -> FeatureExp -> FeatureExp
p /\ q = And p q

(\/) :: FeatureExp -> FeatureExp -> FeatureExp
p \/ q = Or p q

xor :: FeatureExp -> FeatureExp -> FeatureExp
p `xor` q = (p \/ q) /\ (Not (p /\ q))



choose :: [FeatureExp] -> Int -> FeatureExp
choose fs n = fs !! n

chooseMany :: [FeatureExp] -> Int -> FeatureExp
chooseMany fs 0 = B True
chooseMany fs n = And (choose fs n) (chooseMany fs (n-1))


-- tc01 = TestCase (assertEqual "test for exp eval" (eval $ (B True) <=> (B True) /\ (B False) .=> (B True)) True)



-- ["iris" <=> "persistence", (Or $ "nosql" => "persistence" "sql" => "persistence") ,]
