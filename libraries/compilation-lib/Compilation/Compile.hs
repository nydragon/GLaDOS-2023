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
    | otherwise = compileVariable call reg functions
        -- (Call funcName) -> throw Unimplemented
        -- (ExprList ls) -> throw Unimplemented
        -- (Symbole s) -> throw Unimplemented
        -- x -> throw Unimplemented
    where
        compiledVal = compileExpr val
        call = (Ast.Call "define" ((Ast.Symbole s) : val))
compileExpr x reg = throw FatalError

compileProgram' :: Ast.Expr -> Registry -> RetVal
compileProgram' (Ast.ExprList (x:xs)) reg =

compileProgram :: Ast.Expr -> [FunctionBlock]
compileProgram (Ast.ExprList ls) = compileFunc' ls []

-- ─── Function Compilation ────────────────────────────────────────────────────────────────────────

compileCall :: Ast.Expr -> Registry -> RetVal

-- This function attempts to convert an Ast.ExprList to a Ast.Call
-- If the function does not exist, it returns Nothing
-- Args : Input ExprList + Registry
-- Output : Ast.Call
convertToCall :: Ast.Expr -> Registry -> Maybe Ast.Expr
convertToCall (Ast.ExprList (Ast.Symbole name : ls)) reg = if isFunction name
    then Just $ Ast.Call name ls
    else Nothing

-- ─── Variable Compilation ────────────────────────────────────────────────────────────────────────

-- Compiles instructions required for variable compilation
--
-- Note : This does not check the registry if the variable is already defined
-- Args : Ast.Call (Expected to haveproper define form for a variable definition) -> reg -> functions
compileVariable :: Ast.Expr -> Registry -> [FunctionBlock] -> RetVal
compileVariable (Ast.Call "define" ((Ast.Symbole s) : (Ast.ExprList ls))) reg functions = throw Unimplemented
compileVariable (Ast.Call "define" ((Ast.Symbole s) : (Ast.Call name args))) reg functions = throw Unimplemented
compileVariable (Ast.Call "define" ((Ast.Symbole s) : (Ast.Symbole s))) reg functions = throw Unimplemented
compileVariable (Ast.Call "define" ((Ast.Symbole s) : atom)) reg functions = RetVal instructions functions
    where
        instructions = [Init s, Move s $ show atom]
