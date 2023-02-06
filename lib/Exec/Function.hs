module Exec.Function where

import qualified Data.Map as Map
import Control.Exception

import qualified Parsing.Ast as Ast
import Exec.Registry
import Exec.RuntimeException
import Debug.Trace

-- IMPORTANT : This file must NOT be imported in Exec module. (Cyclic Dependencies)

-- ─── Definition And Getting ──────────────────────────────────────────────────────────────────────

-- THIS IS TO BE CALLED FROM THE BUILTINS MODULE

-- Defines a function
-- Returns Nothing if name is alreadt used
--
-- Args : define args -> registry
-- Note : Returns retval as define is a function (returns NULL)
-- EXPECTS : Args to contain two sublists (hence big pattern match)
defineFunc :: [Ast.Expr] -> Registry -> IO RetVal
defineFunc [Ast.Symbole n, Ast.Call "lambda" d] (v, f) = return $ RetVal (v, newReg) Ast.Null
    where newReg = Map.insert n (Ast.ExprList d) f
definefunc _ _ = throwIO $ InvalidFunctionDefinition "<Unknown Function Name>"

-- Get definitions
-- Returns Maybe Function representing (arg names, body function call)
lookupFunc :: String -> FuncRegistry -> Maybe Function
lookupFunc = Map.lookup

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Checks if argument list is valid (only symboles)
isArgumentList :: [Ast.Expr] -> Bool
isArgumentList [] = True
isArgumentList (Ast.Symbole _ : xs) = isArgumentList xs
isArgumentList _ = False
