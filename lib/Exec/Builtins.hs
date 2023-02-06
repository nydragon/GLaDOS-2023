module Exec.Builtins where

import Control.Exception (throwIO)
import Data.Typeable
import Exec.Registry
import Exec.RuntimeException
import qualified Parsing.Ast as Ast
import Exec.Variables
import Exec.Function
import Debug.Trace

-- Function declarations should use the same prototype :
-- [Ast.Expr] -> Registry -> IO RetVal
--
-- The first list is a list of all arguments
-- Registry is the registry
--
-- Returns RetVal

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes an Expr.Call that has been confirmed to be a builtin function
--
-- args : Expr.Call -> Registry
execBuiltin :: Ast.Expr -> Registry -> IO RetVal
execBuiltin (Ast.Call func ls) reg = case func of
  "println" -> printlnBuiltin ls reg
  "print" -> printBuiltin ls reg
  "div" -> divBuiltin ls reg
  "mod" -> modulo ls reg
  "*" -> multiply ls reg
  "-" -> subBuiltin ls reg
  "+" -> add ls reg
  "<" -> lt ls reg
  "<=" -> lte ls reg
  ">" -> gt ls reg
  ">=" -> gte ls reg
  "eq?" -> eq ls reg
  "if" -> ifBuiltin ls reg
  "define" -> distinguishDefine ls reg
  _ -> throwIO NotYetImplemented
execBuiltin _ _ = throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printlnBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
printlnBuiltin ls reg = print (head ls) >> return output
  where
    output = RetVal reg Ast.Null

printBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
printBuiltin ls reg = putStr (show $ head ls) >> return output
  where
    output = RetVal reg Ast.Null

divBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
divBuiltin [Ast.Num a, Ast.Num 0] reg = throwIO NullDivision
divBuiltin [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num (div a b)
divBuiltin [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
divBuiltin [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
divBuiltin _ _ = throwIO $ InvalidArgumentCount "/"

modulo :: [Ast.Expr] -> Registry -> IO RetVal
modulo [Ast.Num a, Ast.Num 0] reg = throwIO NullDivision
modulo [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num (mod a b)
modulo [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
modulo [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
modulo _ _ = throwIO $ InvalidArgumentCount "%"

multiply :: [Ast.Expr] -> Registry -> IO RetVal
multiply [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num ((*) a b)
multiply [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
multiply [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
multiply _ _ = throwIO $ InvalidArgumentCount "*"

subBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
subBuiltin [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num ((-) a b)
subBuiltin [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
subBuiltin [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
subBuiltin _ _ = throwIO $ InvalidArgumentCount "-"

add :: [Ast.Expr] -> Registry -> IO RetVal
add [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num ((+) a b)
add [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
add [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
add arg _ = trace ("Args are s: " ++ show arg) throwIO (InvalidArgumentCount "+")

lt :: [Ast.Expr] -> Registry -> IO RetVal
lt [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Boolean ((<) a b)
lt [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
lt [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
lt _ _ = throwIO $ InvalidArgumentCount "<"

lte :: [Ast.Expr] -> Registry -> IO RetVal
lte [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Boolean ((<=) a b)
lte [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
lte [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
lte _ _ = throwIO $ InvalidArgumentCount "<="

gt :: [Ast.Expr] -> Registry -> IO RetVal
gt [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Boolean ((>) a b)
gt [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
gt [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
gt _ _ = throwIO $ InvalidArgumentCount ">"

gte :: [Ast.Expr] -> Registry -> IO RetVal
gte [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Boolean ((>=) a b)
gte [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
gte [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
gte _ _ = throwIO $ InvalidArgumentCount ">="

eq :: [Ast.Expr] -> Registry -> IO RetVal
eq [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Boolean ((==) a b)
eq [Ast.Boolean a, Ast.Boolean b] reg = return $ RetVal reg $ Ast.Boolean ((==) a b)
eq [a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
eq _ _ = throwIO $ InvalidArgumentCount "eq"

ifBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
ifBuiltin [Ast.Boolean cond, caseT, caseF] reg
  | cond = return $ RetVal reg caseT
  | otherwise = return $ RetVal reg caseF
ifBuiltin [Ast.Boolean a, Ast.Boolean b] reg = return $ RetVal reg $ Ast.Boolean ((==) a b)
ifBuiltin [a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
ifBuiltin _ _ = throwIO $ InvalidArgumentCount "eq"

-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
-- This will most likely have to be moved
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf

-- Utility function to distinguish between variable definition and function definition
-- Args : [Expr] of args -> Registry
distinguishDefine :: [Ast.Expr] -> Registry -> IO RetVal
distinguishDefine [Ast.Symbole s, Ast.ExprList (Ast.Symbole "lambda" : xs)] reg = defineFunc [Ast.Symbole s, Ast.ExprList (Ast.Symbole "lambda" : xs)] reg
distinguishDefine args reg = defineVar args reg
