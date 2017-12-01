module Test.FM.FeatureModelTest where

import Test.HUnit

import Data.Tree

import Data.FM.FeatureModel
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.SAT


ft01 = Node (Feature "iris" BasicFeature Mandatory) []

ft02 = Node (Feature "iris" BasicFeature Mandatory) [
        (Node (Feature "security" OrFeature Mandatory) []),
        (Node (Feature "persists" AltFeature Mandatory) [])
    ]

ft03 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]

ft04 = Node (Feature "iris" BasicFeature Mandatory) [
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

fm01 = FeatureModel ft01 []
fm02 = FeatureModel ft02 []
fm03 = FeatureModel ft03 []
fm04 = FeatureModel ft04 []

fm01expr = fmToFeatureExpressions fm01
fm02expr = fmToFeatureExpressions fm02
fm03expr = fmToFeatureExpressions fm03
fm04expr = fmToFeatureExpressions fm04

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


runFeatureModelTests = runTestTT fm01Tests

------------------- FEATURE MODEL - TEST 02 -------------------
--
--
