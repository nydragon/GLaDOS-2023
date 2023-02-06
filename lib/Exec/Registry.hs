module Exec.Registry where

import qualified Data.Map as Map
import qualified Parsing.Ast as Ast

-- Represents a function
-- Contains: Args List [String], and definition Ast.Call
type Function =  Ast.Expr

-- Function registry
type FuncRegistry = Map.Map String Function

-- Variable registry
type VarRegistry = Map.Map String Ast.Expr

-- ─── Registry ──────────────────────────────────────────────────────────────────────────────────────

type Registry = (VarRegistry, FuncRegistry)

-- Instantiate empty Registry struct
emptyRegistry :: Registry
emptyRegistry = (Map.empty, Map.empty)

-- ─── Return Value ────────────────────────────────────────────────────────────────────────────────

-- Data structure used as main function return type
data RetVal = RetVal Registry Ast.Expr
    deriving (Show)

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- To check if name is already defined
isNameDefined :: String -> Registry -> Bool
isNameDefined name (vars, funcs) = Map.member name vars || Map.member name funcs