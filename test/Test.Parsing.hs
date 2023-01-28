module Test.Parsing where

testLambda :: TestTree
testLambda = assertFailure "BAM"

parsingTests :: TestTree
parsingTests = testGroup "Parsing Tests" [testLambda]
