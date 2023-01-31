module Exec where

import qualified Data.Map as Map

import qualified Parsing.Ast as Ast

-- Data structure used in order to hold a value of a given type
data Atom = Name String
    | Val Integer
    | Boolean Bool

-- Utility type synonyms
type VarLookup = Map String Atom -- Var name, var value
type FuncLookup = Map String ExprList -- Function name, function body
type Registry = (VarLookup, FuncLookup) -- The registry is just a tuple of two maps

-- ─── Function And Var Lookup ─────────────────────────────────────────────────────────────────────

-- Get definitions
lookupVar :: String -> VarLookup -> Maybe Atom
lookupVar name variables = lookup name variables

-- Get definitions
lookupFunc :: String -> FuncLookup -> Maybe ExprList
lookupFunc name functions = lookup name functions

-- To check if name is already defined
isNameDefined :: String -> Registry -> Bool
isNameDefined name (vars, funcs) = member name vars || member name funcs

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: String -> Atom -> Registry -> Maybe Registry
defineVar name val (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (insert name val vars, funcs)

-- Defines a function
-- Returns Nothing if name is alreadt used
defineFunc :: String -> ExprList -> Registry -> Maybe Registry
defineFunc name def (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (vars, insert name def funcs)


-- ─── Main Function ───────────────────────────────────────────────────────────────────────────────

-- This is the main function used in order to evaluate an Aast
--
-- This function implements EXCEPTION HANDLING, hence the IO ()
--
-- Args : input ast -> Functions -> Variables
run' :: [Ast.Ast] -> [[Func]] -> 

-- Entry point function
run :: [Ast.Ast] -> IO ()
run [] = return ()
run (Call sym ls : xs) = -- 