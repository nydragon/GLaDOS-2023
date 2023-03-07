module Compilation.Compile where

import qualified Parsing.Ast as Ast
import Compilation.Function
import Compilation.Utils
import Compilation.RetVal
import FunctionBlock

import Control.Exception
import Data.Maybe
import Debug.Trace

-- ─── Compile Expression ──────────────────────────────────────────────────────────────────────────

-- Compiles and Ast Expression
--
-- This is essentially the main function for compilation,
--    asides from the compileProgram func
-- Args : Expression -> Registry -> Defined Functions
-- Return : (Instructions (if not function def), Updated function definitions)
compileExpr :: Ast.Expr -> Registry -> [FunctionBlock] -> RetVal
-- Lambda called on definition
compileExpr (Ast.ExprList (Ast.Call "lambda" [Ast.ExprList args, body] : argValues)) reg = evalLambda args body argValues reg
-- Basic ExprList
compileExpr (Ast.ExprList ls) reg = evalExprList ls reg
-- Function definition
compileExpr (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg = execCall (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg
-- Variable definition
compileExpr (Ast.Call "define" ((Ast.Symbole s) : val)) reg functions
    | isDeclared s = throw $ VariableAlreadyDefined s
    | otherwise = case val of
        (Call funcName) -> throw Unimplemented
        (ExprList ls) -> throw Unimplemented
        (Symbole s) -> throw Unimplemented
        x -> throw Unimplemented
    where
        compiledVal = compileExpr val
compileExpr x reg = throw FatalError

compileProgram' :: Ast.Expr -> Registry -> ([Instruction], [FunctionBlock])
compileProgram' (Ast.ExprList (x:xs)) reg =

compileProgram :: Ast.Expr -> [FunctionBlock]
compileProgram (Ast.ExprList ls) = compileFunc' ls []
