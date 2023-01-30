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
        parseArgsTest ["binaryName", "", "", ""] (ParsedArgs False "") "No file",
        parseArgsTest ["binaryName", "-h"] (ParsedArgs True "") "Only -h",
        parseArgsTest ["binaryName", "test", "-h"] (ParsedArgs True "test") "File with -h",
        parseArgsTest ["binaryName"] (ParsedArgs False "") "No Args",
        parseArgsTest ["binaryName", "filename"] (ParsedArgs False "filename") "Only filename"
    ]
