module Test.FM.ExpressionTest where

import Test.HUnit

import Data.FM.Expression
import Data.FM.Feature
import Data.FM.Tree
import Data.Tree

fm01 = Node (Feature "iris" BasicFeature Mandatory) [
             (Node (Feature "security" OrFeature Mandatory) [
                (Node (Feature "sha-256" BasicFeature Optional) []),
                (Node (Feature "RSA" BasicFeature Optional) [])
             ]),
             (Node (Feature "persist" AltFeature Mandatory) [
                (Node (Feature "SQL" BasicFeature Optional) []),
                (Node (Feature "NoSQL" BasicFeature Optional) [])
             ])
            ]
