import Data.List

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit
{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

import Parsing

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Unit Tests" [parsingTests]