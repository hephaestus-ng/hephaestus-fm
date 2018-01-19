import Test.FM.FeatureTest
import Test.FM.TreeTest
import Test.FM.ExpressionTest
import Test.FM.FeatureModelTest
import Test.FM.ProductConfigurationTest

import Control.Monad
import Test.HUnit.Base

main :: IO Test.HUnit.Base.Counts
-- main = runFeatureTests
main = do
    a    <- runFeatureTests
    b    <- runFeatureModelTests
    c    <- runProductConfigurationTests
    return b
