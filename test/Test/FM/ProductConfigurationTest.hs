module Test.FM.ProductConfigurationTest where


import Data.FM.FeatureModel
import Data.FM.ProductConfiguration
import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.FM.SAT
import Data.Tree

import Test.HUnit

ft02 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]

fm02 = FeatureModel ft02 []

pc1 = ["iris", "security", "sha-256", "persist", "SQL"]

pc2 = ["iris", "security", "sha-256"]

pc3 = ["iris", "security", "sha-256", "persist", "SQL", "NoSQL"]


test01 = TestCase (assertEqual "Valid ProductConfiguration"
                  True (isValid fm02 pc1))

test02 = TestCase (assertEqual "Not Valid ProductConfiguration"
                  False (isValid fm02 pc2))

test03 = TestCase (assertEqual "Not Valid ProductConfiguration"
                  False (isValid fm02 pc3))

pcTests = TestList
      [
        TestLabel "Product Configuration 1" test01,
        TestLabel "Product Configuration 2" test02,
        TestLabel "Product Configuration 3" test03
      ]
runProductConfigurationTests = runTestTT pcTests
