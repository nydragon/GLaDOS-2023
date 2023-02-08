module Parsing.TokenTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Token

listTokenizeTests = [
        ("empty input", "", []),
        ("space", " ", []),
        ("new line", "\n", []),
        ("single digit", "1", [Num 1]),
        ("mulitple digit", "123", [Num 123]),
        ("single Keyword", "keyword", [Keyword "keyword"]),
        ("open scope", "(", [OpenScope]),
        ("close Scope", ")", [CloseScope]),
        ("1 keyword ( 123 )", "1 keyword ( 123 )", [Num 1, Keyword "keyword", OpenScope, Num 123, CloseScope]),
        ("(define x 2)\\n(+ 3 x)", "(define x 2)\n(+ 3 x)", [OpenScope, Keyword "define", Keyword "x", Num 2, CloseScope, OpenScope, Keyword "+", Num 3, Keyword "x", CloseScope])
    ]

parseTokenTests :: TestTree
parseTokenTests = testGroup "parseToken tests"
  [ testCase "OpenScope" $ parseToken "(" @?= OpenScope
  , testCase "CloseScope" $ parseToken ")" @?= CloseScope
  , testCase "Num" $ parseToken "123" @?= Num 123
  , testCase "Negative Num" $ parseToken "-123" @?= Num (-123)
  , testCase "Keyword" $ parseToken "keyword" @?= Keyword "keyword"
  ]

tokenizeTest :: (String, String, [Token]) -> TestTree
tokenizeTest (name, input, output) = testCase ("Test Tokenize " ++ name)  $
        tokenize input @?= output

tokenize'Test :: (String, String, [Token]) -> TestTree
tokenize'Test (name, input, output) = testCase ("Test Tokenize' " ++ name)  $
        tokenize' input "" @?= output

tokenizeLoop :: [(String, String, [Token])] -> [TestTree]
tokenizeLoop [] = []
tokenizeLoop (x:[]) = [tokenize'Test x, tokenizeTest x]
tokenizeLoop (x:xs) = (tokenize'Test x:tokenizeTest x: tokenizeLoop xs)

tokenizeSuite :: TestTree
tokenizeSuite = testGroup "tokenize tests" (tokenizeLoop listTokenizeTests)

tokenSuite :: TestTree
tokenSuite = testGroup "Parsing.Token Test Suite" [
        parseTokenTests,
        tokenizeSuite
    ]