module Parsing.AstTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parsing.Ast
import qualified Parsing.Cpt as Cpt

parseExprTests :: TestTree
parseExprTests = testGroup "Tests for parseExpr and parseExprList functions" [
    testCase "Parse empty input list" $
        assertEqual "Lists are empty" (parseExprList []) [],
    testCase "Parse input list with single Val" $
        parseExprList [Cpt.Val 1] @?= [Num 1],
    testCase "Parse input list with single Sym" $
        parseExprList [Cpt.Sym "+"] @?= [Symbole "+"],
    testCase "Parse input list with multiple elements" $
        parseExprList [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2] @?= [Symbole "+", Num 1, Num 2],
    testCase "Parse input list with nested list" $
        parseExprList [Cpt.List [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2]] @?= [ExprList [Symbole "+", Num 1, Num 2]],
    testCase "Parse input list with nested list and multiple elements" $
        parseExprList [Cpt.Sym "+", Cpt.Val 1, Cpt.List [Cpt.Sym "*", Cpt.Boolean True, Cpt.Val 3]] @?= [Symbole "+", Num 1, ExprList [Symbole "*", Boolean True, Num 3]],
    testCase "Parse input list using parseExpr with single Sym" $
        parseExpr [Cpt.Sym "+"] @?= ExprList [Symbole "+"],
    testCase "Parse input list using parseExpr with multiple elements" $
        parseExpr [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2] @?= ExprList [Symbole "+", Num 1, Num 2],
    testCase "Parse input list using parseExpr with nested list" $
        parseExpr [Cpt.List [Cpt.Sym "+", Cpt.Val 1, Cpt.Val 2]] @?= ExprList [ExprList [Symbole "+", Num 1, Num 2]],
    testCase "Parse input list using parseExpr with nested list and multiple elements" $
        parseExpr [Cpt.Sym "+", Cpt.Val 1, Cpt.List [Cpt.Sym "*", Cpt.Val 2, Cpt.Val 3]] @?= ExprList [Symbole "+", Num 1, ExprList [Symbole "*", Num 2, Num 3]]
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