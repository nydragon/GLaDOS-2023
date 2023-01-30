import Data.List
import ParseTests

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.HUnit

{-# OPTIONS_GHC -F -pgmF tasty-discover#-}

main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Glados Unit Tests" [
        parseSuite
    ]