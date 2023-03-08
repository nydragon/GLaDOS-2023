module CompilationLib where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ()

import CompilationLib.Parser.CptTests ( cptSuite )
import CompilationLib.Parser.ArgsTests ( argsSuite )
import CompilationLib.Parser.AstTests ( astSuite )
import CompilationLib.Parser.TokenTests ( tokenSuite )
import CompilationLib.Parser.InfixTests ( infixSuite )
import CompilationLib.Compilation.CompilationTests (testCompileConditional)

compilationLibSuite :: TestTree
compilationLibSuite = testGroup "Parsing Testsuite" [
        argsSuite,
        tokenSuite,
        cptSuite,
        astSuite,
        infixSuite,
        testCompileConditional
    ]