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
argsSuite = testGroup "Parsing.Args Test Suite" [
        parseArgsTest ["binaryName", "", ""] (ParsedArgs False "") "Empty Args",
        parseArgsTest ["binaryName"] (ParsedArgs False "") "No Args",
        parseArgsTest ["binaryName", "-h"] (ParsedArgs True "") "Only -h",
        parseArgsTest ["binaryName", "filename"] (ParsedArgs False "filename") "Only filename",
        parseArgsTest ["binaryName", "filename", "-h"] (ParsedArgs True "filename") "File with -h"
    ]
