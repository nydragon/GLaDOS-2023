module Compilation.Compile where

import qualified Parsing.Ast as Ast
import Compilation.Function
import Compilation.Utils
import Compilation.RetVal
import Compilation.Registry
import Compilation.CompilationError
import FunctionBlock
import Instruction

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
compileExpr :: Ast.Expr -> Registry -> RetVal
-- Lambda called on definition
-- compileExpr (Ast.ExprList (Ast.Call "lambda" [Ast.ExprList args, body] : argValues)) reg funcs = evalLambda args body argValues reg
-- Basic ExprList
-- compileExpr (Ast.ExprList ls) reg funcs = evalExprList ls reg
-- Function definition
-- compileExpr (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg funcs = execCall (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg
-- Variable definition
compileExpr (Ast.Call "define" ((Ast.Symbole s) : val)) reg
    | isDeclared s reg = throw $ VariableAlreadyDefined s
    | otherwise = compileVariable call reg
    where
        call = Ast.Call "define" (Ast.Symbole s : val)
compileExpr x reg = throw FatalError
-- compileExpr _ _ funcs = throw Unimplemented

compileProgram' :: Ast.Expr -> Registry -> RetVal
compileProgram' (Ast.ExprList (x:xs)) reg = concatRetVal compiledLeftover compiledExpr
    where
        (f, v) = reg
        compiledLeftover = compileProgram' (Ast.ExprList xs) reg
        (RetVal _ _ updatedReg) = compiledLeftover
        compiledExpr = compileExpr x updatedReg

compileProgram :: Ast.Expr -> RetVal
compileProgram (Ast.ExprList ls) = compileProgram' list emptyRegistry
    where
        list = Ast.ExprList ls
compileProgram _ = throw FatalError

-- ─── Function Compilation ────────────────────────────────────────────────────────────────────────

-- compileCall :: Ast.Expr -> Registry -> RetVal

-- This function attempts to convert an Ast.ExprList to a Ast.Call
-- If the function does not exist, it returns Nothing
-- Args : Input ExprList + Registry
-- Output : Ast.Call
convertToCall :: Ast.Expr -> Registry -> Maybe Ast.Expr
convertToCall (Ast.ExprList (Ast.Symbole name : ls)) reg = if isFunction name reg
    then Just $ Ast.Call name ls
    else Nothing

-- ─── Variable Compilation ────────────────────────────────────────────────────────────────────────

-- Compiles instructions required for variable compilation
--
-- Note : This does not check the registry if the variable is already defined
-- Args : Ast.Call (Expected to haveproper define form for a variable definition) -> reg -> functions
compileVariable :: Ast.Expr -> Registry -> RetVal
compileVariable (Ast.Call "define" [Ast.Symbole s, Ast.ExprList ls]) reg = throw Unimplemented
compileVariable (Ast.Call "define" [Ast.Symbole s, Ast.Call name args]) reg = throw Unimplemented
compileVariable (Ast.Call "define" [Ast.Symbole s1, Ast.Symbole s2]) reg = throw Unimplemented
compileVariable (Ast.Call "define" [Ast.Symbole s, atom]) reg = RetVal instructions [] reg
    where
        instructions = [Init s, Move s $ show atom]
