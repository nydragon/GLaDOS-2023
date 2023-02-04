module Exec.Lookup where

import qualified Data.Map as Map
import Data.Typeable
import Control.Exception
import Data.Maybe

import qualified Parsing.Ast as Ast
import Exec.RuntimeException

-- Represents a function
-- Contains: Args List [String], and definition Ast.Call
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

-- Defines a function
-- Returns Nothing if name is alreadt used
--
-- Args : define args -> registry
-- Note : Returns retval as define is a function (returns NULL)
-- EXPECTS : Args to contain two sublists (hence big pattern match)
defineFunc :: [Ast.Expr] -> Registry -> IO RetVal
defineFunc (Ast.ExprList (Ast.Symbole n : args) : Ast.ExprList def : _) (v, f) =
    if isArgumentList args then
        case Ast.exprListToCall def of
            Nothing -> throwIO $ InvalidFunctionDefinition n
            -- Updated function registry and return Null
            Just x -> return $ RetVal (v, Map.insert n (argNameStr, x) f) Ast.Null
    else throwIO $ InvalidFunctionDefinition n
        where   argNameStr = [str | Ast.Symbole str <- args]
definefunc _ _ = throwIO $ InvalidFunctionDefinition "<Unknown Function Name>"

-- ─── Utility ─────────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf

-- Returns boolean if Expr is atomic. This means it cannot be further reduced.
-- Note: Sym is not atomic as it needs to be reduced to a value
isAtomic :: Ast.Expr -> Bool
isAtomic (Ast.Num _) = True
isAtomic (Ast.Boolean _) = True
isAtomic Ast.Null = True
isAtomic _ = False

isListAtomic :: [Ast.Expr] -> Bool
isListAtomic = foldr ((&&) . isAtomic) True

isArgumentList :: [Ast.Expr] -> Bool
isArgumentList [] = True
isArgumentList (Ast.Symbole _ : xs) = True && isArgumentList xs
isArgumentList _ = False