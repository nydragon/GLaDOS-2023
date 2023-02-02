module Parsing.ArgsTests where

import           Parsing
import           Parsing.Args
import           Test.Tasty
import           Test.Tasty.HUnit

parseArgsTest :: [String] -> Options -> [String] -> String-> TestTree
parseArgsTest arr restest nonargs testName = testCase testName $ do
    (res,fs) <- parse arr
    restest @=? res
    nonargs @=? fs

argsSuite :: TestTree
argsSuite = testGroup "Parsing.Args Test Suite" [
        parseArgsTest [] Options {
            file       = Nothing,
            help       = False,
            debug      = False
        } [] "Empty Args",
        parseArgsTest ["filename"] Options {
            file       = Nothing,
            help       = False,
            debug      = False
        } ["filename"] "File via non flag params",
        parseArgsTest ["filename", "-d"]  Options {
            file       = Nothing,
            help       = False,
            debug      = True
        } ["filename"] "Short debug flag",
        parseArgsTest ["--file=filename"] Options {
            file       = Just "filename",
            help       = False,
            debug      = False
        } [] "Filename via flag",
        parseArgsTest ["--debug"] Options {
            file       = Nothing,
            help       = False,
            debug      = True
        } [] "Long debug flag"    ]
