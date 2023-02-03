module Exec.Builtins where

import Exec.Lookup
import qualified Parsing.Ast as Ast
import Control.Exception (throwIO)
import Exec.RuntimeException

-- Function declarations should use the same prototype :
-- [Ast.Expr] -> Lookup -> IO RetVal
--
-- The first list is a list of all arguments
-- Lookup is the registry
--
-- Returns RetVal

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes an Expr.Call that has been confirmed to be a builtin function
--
-- args : Expr.Call -> Lookup
execBuiltin :: Ast.Expr -> Lookup -> IO RetVal
execBuiltin (Ast.Call func ls) reg = case func of
    "println" -> printBuiltin ls reg
    _ -> throwIO NotYetImplemented
execBuiltin _ _ = throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printBuiltin :: [Ast.Expr] -> Lookup -> IO RetVal
printBuiltin ls reg = print (head ls) >> return output
    where   output = RetVal reg Nothing

div :: [Ast.Expr] -> Lookup -> IO RetVal
div (Ast.Num a : Ast.Num b : _) reg = if b == 0 then throwIO NullDivision else return output
    where   output = RetVal reg (Just  $ Val (a / b))
div (Ast.Num a : x : xs) _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName x)

modulo :: Integer -> Integer -> Either Integer String
modulo a 0 = Right "ZeroDivisionError"
modulo a b = Left (mod a b)

multiply :: Integer -> Integer -> Integer
multiply a b = a * b

substract :: Integer -> Integer -> Integer
substract a b = a - b

add :: Integer -> Integer ->   Integer

add a b = a + b

lt :: (Ord a) => a -> a -> Bool
lt a b = a < b