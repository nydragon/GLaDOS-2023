module Exec where

import qualified Data.Map as Map

import qualified Parsing.Ast as Ast

-- Data structure used in order to hold a value of a given type
data Atom = Name String
    | Val Integer
    | Boolean Bool

-- Function to parse a 

-- Utility type synonyms
type VarLookup = Map.Map String Atom -- Var name, var value
type FuncLookup = Map.Map String Ast.Expr -- Function name, function body
type Registry = (VarLookup, FuncLookup) -- The registry is just a tuple of two maps

-- ─── Function And Var Lookup ─────────────────────────────────────────────────────────────────────

-- Get definitions
lookupVar :: String -> VarLookup -> Maybe Atom
lookupVar = Map.lookup

-- Get definitions
-- Returns an ExprList representing body of function
lookupFunc :: String -> FuncLookup -> Maybe Ast.Expr
lookupFunc = Map.lookup

-- To check if name is already defined
isNameDefined :: String -> Registry -> Bool
isNameDefined name (vars, funcs) = Map.member name vars || Map.member name funcs

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: String -> Atom -> Registry -> Maybe Registry
defineVar name val (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (Map.insert name val vars, funcs)

-- Defines a function
-- Returns Nothing if name is alreadt used
-- Args: function name -> Function body -> Registry
defineFunc :: String -> Ast.Expr -> Registry -> Maybe Registry
defineFunc name (Ast.ExprList ls) (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        def = Ast.ExprList ls
        newReg = (vars, Map.insert name def funcs)
defineFunc _ _ _ = Nothing -- Invalid function body (nor ExprList)


-- ─── Main Function ───────────────────────────────────────────────────────────────────────────────
