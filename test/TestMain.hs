import Data.List

-- Test framework
import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

testLambda :: TestTree
testLambda = testCase "Exemple" $ do
    2 + 2 @?= 4

parsingTests :: TestTree
parsingTests = testGroup "Parsing Tests" [testLambda]

allTests :: TestTree
allTests = testGroup "Unit Tests" [parsingTests]