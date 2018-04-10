import Test.FM.ExpressionTest
import Test.FM.FeatureModelTest
import Test.FM.ProductConfigurationTest
-- import Test.Parser.XMLTest

import Control.Monad
import Test.HUnit.Base

main :: IO Test.HUnit.Base.Counts
main = do
    a    <- runExpressionTests
    b    <- runFeatureModelTests
    c    <- runProductConfigurationTests
    -- d    <- runXMLParserTests
    return c
