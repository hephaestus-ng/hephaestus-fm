module Test.FM.FeatureModelTest where

import Test.HUnit

import Control.Lens
import Data.Tree
import Data.Tree.Lens

import Data.FM.FeatureModel
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.SAT



------------------- FEATURE MODEL - TEST 01 -------------------
--
--
ft01 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]



derivationTree = Node (Feature "iris" BasicFeature Mandatory) [
                    (Node (Feature "security" OrFeature Mandatory) [
                      (Node (Feature "sha-256" BasicFeature Optional) [])
                    ])
                  ]

derivationExp = featureTreeToExp derivationTree

derivationModel = FeatureModel ft01 derivationExp


fm01 = FeatureModel ft01 []

fm02 = FeatureModel ft01 [Not (Ref "iris")]

fm03 = FeatureModel ft01 [Not (Ref "persist")]

fm04 = FeatureModel ft01 [And (Not (Ref "SQL")) (Not (Ref "NoSQL"))]


test01 = TestCase (assertEqual "SAT - satisfiable"
                  True (satSolver fm01))

test02 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm02))

test03 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm03))

test04 = TestCase (assertEqual "SAT - unsatisfiable"
                  False (satSolver fm04))


fm01Tests = TestList
      [
        TestLabel "SAT solving 1" test01,
        TestLabel "SAT solving 2" test02,
        TestLabel "SAT solving 3" test03,
        TestLabel "SAT solving 4" test04
      ]

fm01RunTests = runTestTT fm01Tests

------------------- FEATURE MODEL - TEST 02 -------------------
--
--
ft02 = Node (Feature "iris" BasicFeature Mandatory) [
         (Node (Feature "security" OrFeature Mandatory) [
            (Node (Feature "sha-256" BasicFeature Mandatory) []),
            (Node (Feature "RSA" BasicFeature Mandatory) [])
         ]),
         (Node (Feature "persist" AltFeature Mandatory) [
            (Node (Feature "SQL" AltFeature Mandatory) [
              (Node (Feature "PostgreSQL" BasicFeature Optional) []),
              (Node (Feature "MySQL" BasicFeature Optional) []),
              (Node (Feature "LiteSQL" BasicFeature Optional) [])
            ]),
            (Node (Feature "NoSQL" BasicFeature Mandatory) [
              (Node (Feature "MongoDB" BasicFeature Mandatory) [])
            ])
         ])
        ]
