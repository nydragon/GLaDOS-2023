module ParseArgsTest where

import Parsing
import Parsing.Args
import Test.Tasty
import Test.Tasty.HUnit

parseArgsTest :: [String] -> ParsedArgs -> String-> TestTree
parseArgsTest arr restest testName = testCase testName $ do
    res <- parseArgs' arr 0 (ParsedArgs False "")
    restest @=? res

argsSuite :: TestTree
argsSuite = testGroup "Parser Args Test Suite" [
        parseArgsTest ["", "try", ""] (ParsedArgs False "try") "Only file",
        parseArgsTest ["", "-h"] (ParsedArgs True "") "Only -h",
        parseArgsTest ["", "test", "-h"] (ParsedArgs True "test") "File with -h",
        parseArgsTest [""] (ParsedArgs False "") "No Args"
    ]
