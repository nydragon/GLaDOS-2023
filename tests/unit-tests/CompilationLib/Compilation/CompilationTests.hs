module CompilationLib.Compilation.CompilationTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?), assertEqual)
import qualified Parsing.Ast as Ast
import Compilation.RetVal (RetVal(RetVal))
import Parsing.Ast (Expr(Num))
import qualified Exec.InferType as Type
import qualified Instruction as Type
import Compilation.Compile (compileConditional)
import Instruction (Instruction(Push))

testCompileConditional :: TestTree
testCompileConditional = testGroup "compileConditional"
  [ testCase "simple conditional" $
      let ast = Ast.Call "if" [ Ast.Boolean True, Ast.Num 1, Ast.Num 2 ]
          reg = (["foo"], [])
          RetVal instrs blocks reg' = compileConditional ast reg
      in do
        assertEqual "instructions" [Type.Conditional [Push "#t"] [Push "1"] [Push "2"]] instrs
  ]