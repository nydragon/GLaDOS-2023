module CompilationLib.Compilation.CompilationTest where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import qualified Parsing.Ast as Ast

testCompileConditional :: TestTree
testCompileConditional = testGroup "compileConditional"
  [ testCase "simple conditional" $
      let ast = Ast.Call "if" [ Ast.Boolean True, Ast.Num 1, Ast.Num 2 ]
          reg = (["foo"], [])
          RetVal instrs blocks reg' = compileConditional ast reg
      in do
        assertEqual "instructions" [Conditional (Boolean True) (Num 1) (Num 2)] instrs
        assertEqual "blocks" [] blocks
        assertEqual "registry" reg reg'
  ]