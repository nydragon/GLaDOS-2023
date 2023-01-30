import Data.List

import TokenizationTest

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Unit Tests" [tokenSuite]