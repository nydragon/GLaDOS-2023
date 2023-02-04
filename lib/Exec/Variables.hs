module Exec.Variables where

import qualified Parsing.Ast as Ast
import Exec.RuntimeException
import Exec.Registry
import Exec.Eval

-- Get definitions
lookupVar :: String -> VarRegistry -> Maybe Ast.Expr
lookupVar = Map.lookup

-- Remove a variable definition
removeVar :: String -> Registry -> Registry
removeVar name (vars, funcs) = newReg
    where
        newVars = Map.delete name vars
        newReg = (newVars, funcs)

-- Remove lists of variable definitions
removeVars :: [String] -> Registry -> Registry
removeVars [] reg = reg
removeVars (x : xs) (vars, funcs) = removeVars xs newReg
    where
        newVars = Map.delete name vars
        newReg = (newVars, funcs)

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: [Ast.Expr] -> Registry -> IO RetVal
definevar ((Sym varName : xs) : (expr : ys) : _) (v, f) =
    if isNameDefined varName
        then throwIO AlreadyDefined
        else ret
    where   value = eval expr
            updatedRegistry = Map.insert varName value
            ret = RetVal (updatedRegistry, f) Ast.Null