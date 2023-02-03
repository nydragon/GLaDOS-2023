module Exec.Lookup where

import qualified Data.Map as Map
import Data.Typeable

import qualified Parsing.Ast as Ast

-- Represents a function
-- Contains: Args List [String], and definition ExprList
type Function = ([String], Ast.Expr)
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

-- ─── Function And Var Registry ─────────────────────────────────────────────────────────────────────

-- Get definitions
lookupVar :: String -> VarRegistry -> Maybe Ast.Expr
lookupVar = Map.lookup

-- Get definitions
-- Returns an ExprList representing body of function
lookupFunc :: String -> FuncRegistry -> Maybe Function
lookupFunc = Map.lookup

-- To check if name is already defined
isNameDefined :: String -> Registry -> Bool
isNameDefined name (vars, funcs) = Map.member name vars || Map.member name funcs

-- Defines a variable
-- Returns Nothing if name is already used
defineVar :: String -> Ast.Expr -> Registry -> Maybe Registry
defineVar name val (vars, funcs) = if isNameDefined name reg then Nothing else Just newReg
    where
        reg = (vars, funcs)
        newReg = (Map.insert name val vars, funcs)

-- Remove a variable definition
removeVar :: String -> Registry -> Registry
removeVar name (vars, funcs) = newReg
    where
        newVars = Map.delete name vars
        newReg = (newVars, funcs)

-- Defines a function
-- Returns Nothing if name is alreadt used
-- Args: function name -> Function args -> Function body -> Registry
defineFunc :: String -> [String] -> Ast.Expr -> Registry -> Maybe Registry
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

-- Returns boolean if Expr is atomic. This means it cannot be further reduced.
isAtomic :: Ast.Expr -> Bool
isAtomic (Ast.Num _) = True
isAtomic (Ast.Boolean _) = True
isAtomic (Ast.Symbole _) = True -- Not sure about this one
isAtomic Ast.Null = True
isAtomic _ = False