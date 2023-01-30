module ArgsTest where

import Parsing
import Parsing.Args
import Test.Tasty
import Test.Tasty.HUnit

parseArgsTest :: [String] -> ParsedArgs -> TestTree
parseArgsTest arr restest = testCase "test parse args" $ do
    res <- parseArgs' arr 0 (ParsedArgs False "")
    restest @=? res

argsSuite :: TestTree
argsSuite = testGroup "Unit Tests" [
        parseArgsTest ["", "try", ""] (ParsedArgs False "try"),
        parseArgsTest ["", "test", "-h"] (ParsedArgs True "test"),
        parseArgsTest [""] (ParsedArgs False "")
    ]
