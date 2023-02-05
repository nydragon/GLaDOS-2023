module Exec.Variables where

import qualified Data.Map as Map
import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.RuntimeException
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

-- Remove lists of variable definitions
removeVars :: [String] -> Registry -> Registry
removeVars [] reg = reg
removeVars (x : xs) (vars, funcs) = removeVars xs newReg
    where
        newVars = Map.delete x vars
        newReg = (newVars, funcs)

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: [Ast.Expr] -> Registry -> IO RetVal
defineVar (Ast.ExprList (Ast.Symbole varName : xs) : Ast.ExprList (expr : ys) : _) (v, f) =
    if isNameDefined varName (v, f)
        then throwIO AlreadyDefined
        else return ret
    where
        updatedRegistry = Map.insert varName expr v
        ret = RetVal (updatedRegistry, f) Ast.Null