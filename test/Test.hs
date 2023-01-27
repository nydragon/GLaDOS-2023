import Data.List
import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain tests

tests :: TestTree
tests =testGroup "UnitTests"
    [testCase "first" $
        2+2 @?= 4
    ]