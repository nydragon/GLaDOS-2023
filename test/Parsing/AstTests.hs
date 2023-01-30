module Parsing.AstTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Ast
import Parsing.Cpt

parseExprTests :: TestTree
parseExprTests = testGroup "Tests for parseExpr and parseExprList functions" [
    testCase "Parse empty input list" $
        assertEqual "Lists are empty" (parseExprList []) [],
    testCase "Parse input list with single Val" $
        parseExprList [Val 1] @?= [Num 1],
    testCase "Parse input list with single Sym" $
        parseExprList [Sym "+"] @?= [Symbole "+"],
    testCase "Parse input list with multiple elements" $
        parseExprList [Sym "+", Val 1, Val 2] @?= [Symbole "+", Num 1, Num 2],
    testCase "Parse input list with nested list" $
        parseExprList [List [Sym "+", Val 1, Val 2]] @?= [ExprList [Symbole "+", Num 1, Num 2]],
    testCase "Parse input list with nested list and multiple elements" $
        parseExprList [Sym "+", Val 1, List [Sym "*", Val 2, Val 3]] @?= [Symbole "+", Num 1, ExprList [Symbole "*", Num 2, Num 3]],
    testCase "Parse input list using parseExpr with single Sym" $
        parseExpr [Sym "+"] @?= ExprList [Symbole "+"],
    testCase "Parse input list using parseExpr with multiple elements" $
        parseExpr [Sym "+", Val 1, Val 2] @?= ExprList [Symbole "+", Num 1, Num 2],
    testCase "Parse input list using parseExpr with nested list" $
        parseExpr [List [Sym "+", Val 1, Val 2]] @?= ExprList [ExprList [Symbole "+", Num 1, Num 2]],
    testCase "Parse input list using parseExpr with nested list and multiple elements" $
        parseExpr [Sym "+", Val 1, List [Sym "*", Val 2, Val 3]] @?= ExprList [Symbole "+", Num 1, ExprList [Symbole "*", Num 2, Num 3]]
    ]

exprListToCallTests :: TestTree
exprListToCallTests = testGroup "Tests for exprListToCall function" [
    testCase "Input list is empty" $
        exprListToCall [] @?= Nothing,
    testCase "Input list starts with a Symbole" $
        exprListToCall [Symbole "add", Num 3, Num 4] @?= Just (Call "add" [Num 3, Num 4]),
    testCase "Input list does not start with a Symbole" $
        exprListToCall [Num 3, Num 4] @?= Nothing
    ]


astSuite :: TestTree
astSuite = testGroup "Parsing.Ast Test Suite" [
        parseExprTests,
        exprListToCallTests
    ]