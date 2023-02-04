module Exec.Variables where

import qualified Parsing.Ast as Ast
import Exec.Registry

-- Get definitions
lookupVar :: String -> VarRegistry -> Maybe Ast.Expr
lookupVar = Map.lookup

-- Remove a variable definition
removeVar :: String -> Registry -> Registry
removeVar name (vars, funcs) = newReg
    where
        newVars = Map.delete name vars
        newReg = (newVars, funcs)

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: [Ast.Expr] -> Registry -> IO RetVal
definevar ((Sym varName : xs) : (expr : xs) : xs) (v, f) =
    where   ret = RetVal (Map.insert varName (exal ), f) Ast.Null