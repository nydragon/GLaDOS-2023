module Exec.Lookup where

import qualified Data.Map as Map
import qualified Parsing.Ast as Ast

-- Data structure used in order to hold a value of a given type
data Atom = Name String
    | Val Integer
    | Boolean Bool
    deriving (Show, Eq)


-- Utility type synonyms

-- Represents a function
-- Contains: Args List [String], and definition ExprList
type Function = ([String], Ast.Expr)

-- Variable registry
type VarLookup = Map.Map String Atom
-- Function registry
type FuncLookup = Map.Map String Function

type Lookup = (VarLookup, FuncLookup)

-- Instantiate empty lookup struct
emptyLookup :: Lookup
emptyLookup = (Map.empty, Map.empty)

-- ─── Function And Var Lookup ─────────────────────────────────────────────────────────────────────

-- Get definitions
lookupVar :: String -> VarLookup -> Maybe Atom
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
defineVar :: String -> Atom -> Lookup -> Maybe Lookup
defineVar name val (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (Map.insert name val vars, funcs)

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