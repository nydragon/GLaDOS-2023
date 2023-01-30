module Parsing.TokenTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Token

parseTokenTests :: TestTree
parseTokenTests = testGroup "parseToken tests"
  [ testCase "OpenScope" $ parseToken "(" @?= OpenScope
  , testCase "CloseScope" $ parseToken ")" @?= CloseScope
  , testCase "Num" $ parseToken "123" @?= Num 123
  , testCase "Negative Num" $ parseToken "-123" @?= Num (-123)
  , testCase "Keyword" $ parseToken "keyword" @?= Keyword "keyword"
  ]

tokenizeTests :: TestTree
tokenizeTests = testGroup "tokenize tests"
  [ testCase "tokenize' empty input" $
        tokenize' "" "" @?= [],
    testCase "tokenize' space" $
        tokenize' " " "" @?= [],
    testCase "tokenize' newline" $
        tokenize' "\n" "" @?= [],
    testCase "tokenize' single digit" $
        tokenize' "1" "" @?= [Num 1],
    testCase "tokenize' multiple digits" $
        tokenize' "123" "" @?= [Num 123],
    testCase "tokenize' keyword" $
        tokenize' "keyword" "" @?= [Keyword "keyword"],
    testCase "tokenize' open scope" $
        tokenize' "(" "" @?= [OpenScope],
    testCase "tokenize' close scope" $
        tokenize' ")" "" @?= [CloseScope],
    testCase "tokenize' mixed input" $
        tokenize' "1 keyword ( 123 )" "" @?= [Num 1, Keyword "keyword", OpenScope, Num 123, CloseScope],
    testCase "tokenize" $
        tokenize "1 keyword ( 123 )" @?= [Num 1, Keyword "keyword", OpenScope, Num 123, CloseScope]
  ]

tokenSuite :: TestTree
tokenSuite = testGroup "Parsing.Token Test Suite" [
        parseTokenTests,
        tokenizeTests
    ]