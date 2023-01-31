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

-- ─── Function And Var Lookup ─────────────────────────────────────────────────────────────────────

lookupVar :: String -> VarLookup -> Maybe Atom
lookupVar name variables = lookup name variables

lookupFunc :: String -> FuncLookup -> Maybe ExprList
lookupFunc name functions = lookup name functions

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