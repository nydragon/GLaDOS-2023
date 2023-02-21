module Parsing.InfixTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import qualified Parsing.Cpt as Cpt
import Parsing.Infix (insertLists, infixToPrefix')
import Parsing.Token (tokenize, Token)
import Parsing.Cpt (tokenToCpt)

insertListsTestF :: String -> [Cpt.Cpt] -> [Cpt.Cpt] -> TestTree
insertListsTestF testName arr restest = testCase testName $ do
  let res = insertLists arr
  restest @=? res

infixToPrefixTestF :: String -> [Cpt.Cpt] -> [Cpt.Cpt] -> TestTree
infixToPrefixTestF testName arr restest = testCase testName $ do
  let res = infixToPrefix' arr
  restest @=? res

insertListsTest :: TestTree
insertListsTest =
  testGroup
    "Parsing.InfixTest insertLists"
    [
        insertListsTestF "Succesfull"
          [Cpt.Sym "+", Cpt.Sym "-", Cpt.Val 3, Cpt.Val 2, Cpt.Val 5]
          [Cpt.Sym "+", Cpt.List [Cpt.Sym "-", Cpt.Val 3, Cpt.Val 2], Cpt.Val 5],
          insertListsTestF "Succesfull Complex"
          [Cpt.Sym "+", Cpt.Sym "*", Cpt.Val 3, Cpt.Val 2, Cpt.Sym "/", Cpt.Val 5, Cpt.Sym "-", Cpt.Val 8, Cpt.Val 2]
          [Cpt.Sym "+", Cpt.List [Cpt.Sym "*", Cpt.Val 3, Cpt.Val 2], Cpt.List [Cpt.Sym "/", Cpt.Val 5, Cpt.List [Cpt.Sym "-", Cpt.Val 8, Cpt.Val 2]]],
          insertListsTestF "Succesfull Complex 2"
          [Cpt.Sym "-", Cpt.Sym "*", Cpt.Val 4, Cpt.Val 3, Cpt.Sym "+", Cpt.Sym "/", Cpt.Val 6, Cpt.Val 3, Cpt.Val 5]
          [Cpt.Sym "-", Cpt.List [Cpt.Sym "*", Cpt.Val 4, Cpt.Val 3], Cpt.List [Cpt.Sym "+", Cpt.List [Cpt.Sym "/", Cpt.Val 6, Cpt.Val 3], Cpt.Val 5]]
    ]

infixToPrefixTest :: TestTree
infixToPrefixTest =
  testGroup
    "Parsing.InfixTest infixToPrefix"
    [
        infixToPrefixTestF "Succesfull Simple"
          [Cpt.Val 3, Cpt.Sym "+", Cpt.Val 5]
          [Cpt.Sym "+", Cpt.Val 3, Cpt.Val 5],
        infixToPrefixTestF "Succesfull Simple 2"
          [Cpt.Val 3, Cpt.Sym "+", Cpt.Val 5, Cpt.Sym "*", Cpt.Val 9]
          [Cpt.Sym "+", Cpt.Sym "*", Cpt.Val 3, Cpt.Val 5, Cpt.Val 9],
        infixToPrefixTestF "Succesfull Complex"
          [Cpt.Val 4, Cpt.Sym "*", Cpt.Val 3, Cpt.Sym "-", Cpt.Val 6, Cpt.Sym "/", Cpt.Val 3, Cpt.Sym "+", Cpt.Val 5]
          [Cpt.Sym "-", Cpt.Sym "*", Cpt.Val 4, Cpt.Val 3, Cpt.Sym "+", Cpt.Sym "/", Cpt.Val 6, Cpt.Val 3, Cpt.Val 5]
    ]


infixSuite :: TestTree
infixSuite = testGroup "Parsing.Infix Test Suite" [
        insertListsTest,
        infixToPrefixTest
    ]