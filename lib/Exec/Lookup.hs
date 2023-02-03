module Exec.Lookup where

import qualified Data.Map as Map
import Data.Typeable

import qualified Parsing.Ast as Ast

-- Utility type synonyms

-- Represents a function
-- Contains: Args List [String], and definition ExprList
type Function = ([String], Ast.Expr)

-- Variable registry
type VarLookup = Map.Map String Ast.Expr
-- Function registry
type FuncLookup = Map.Map String Function

type Lookup = (VarLookup, FuncLookup)

-- Data structure used as main function return type
data RetVal = RetVal Lookup (Maybe Ast.Expr)

-- Instantiate empty lookup struct
emptyLookup :: Lookup
emptyLookup = (Map.empty, Map.empty)

-- ─── Function And Var Lookup ─────────────────────────────────────────────────────────────────────

-- Get definitions
lookupVar :: String -> VarLookup -> Maybe Ast.Expr
lookupVar = Map.lookup

-- Get definitions
-- Returns an ExprList representing body of function
lookupFunc :: String -> FuncLookup -> Maybe Function
lookupFunc = Map.lookup

-- To check if name is already defined
isNameDefined :: String -> Lookup -> Bool
isNameDefined name (vars, funcs) = Map.member name vars || Map.member name funcs

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: String -> Ast.Expr -> Lookup -> Maybe Lookup
defineVar name val (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (Map.insert name val vars, funcs)

-- Remove a variable definition
removeVar :: String -> Lookup -> Lookup
removeVar name (vars, funcs) = newReg
    where
        newVars = Map.delete name vars
        newReg = (newVars, funcs)

-- Defines a function
-- Returns Nothing if name is alreadt used
-- Args: function name -> Function args -> Function body -> Lookup
defineFunc :: String -> [String] -> Ast.Expr -> Lookup -> Maybe Lookup
defineFunc name args (Ast.ExprList ls) (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        def = Ast.ExprList ls
        newReg = (vars, Map.insert name (args, def) funcs)
defineFunc _ _ _ _ = Nothing -- Invalid function body (nor ExprList)

-- ─── Utility ─────────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf