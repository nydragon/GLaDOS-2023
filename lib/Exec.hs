module Exec where

import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.Lookup
import Exec.RuntimeException
import Exec.Builtins

-- ─── Execute Function ────────────────────────────────────────────────────────────────────────────

-- Bind all arguments to their values in preparation of a function call
bindArgs :: [String] -> [Ast.Expr] -> Registry -> IO Registry
bindArgs (x : xs) (y : ys) (v, f)= bindArgs xs ys 
    where   updatedReg = (v, )


-- Executes function call
-- Note: Arguments do not need to have been reduced, execFunc takes care of it
execFunc :: String -> [Ast.Expr] -> Registry -> IO RetVal
execFunc funcName args reg
    | isValidBuiltin funcName = execBuiltin atomicArgs
    | case lookupFunc funcName of
        Nothing -> throwIO $ InvalidFunctionCall funcName
        Just (args, def) -> case def of
            Ast.Call cName cArgs -> execCall (Ast.Call cName atomicCallArgs) (bindArgs)
    -- Args reduced to atomic form
    where   atomicArgs = eval args
            atomicCallArgs = eval cArgs
            oldReg = reg    -- Reg without function variables bound
            newReg = 

-- Syntactic sugar to convert Ast.Call to args for execFunc
--
-- Will throw exception if not Ast.Call
execCall :: Ast.Expr -> Registry -> IO RetVal
execCall (Ast.Call n args) reg = execFunc n args
execCall _ _ = throwIO $ InvalidFunctionCall "<Unknown Function Name>"

-- ─── Evaluate Expression ─────────────────────────────────────────────────────────────────────────

-- Evaluates a given expression into an atom
eval :: [Ast.Expr] -> Registry -> IO RetVal
eval (Call func ls : xs) reg = -- INPUT SHOULDN'T BE LIST
-- LIST NEEDS TO BE CHECKED IF FUNCTION CALL
