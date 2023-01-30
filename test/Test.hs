import Data.List
import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

import TokenizationTest

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain tests

tests :: TestTree
tests =testGroup "BABABABABABABABABABABABABABA"
    [testCase "first" $
        2+2 @?= 4,
    tokenSuite
    ]