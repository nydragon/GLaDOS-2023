module Exec.Builtins where

import Control.Exception (throwIO)
import Data.Typeable ( Typeable, typeOf )
-- import GHC.IO.FD ()
-- import System.IO
--     ( hGetContents,
--       IOMode(ReadWriteMode),
--       hSetBuffering,
--       BufferMode(LineBuffering, NoBuffering),
--       stdout )
-- import GHC.IO.Handle.FD ( openFile )
-- import Foreign.C.Types ()
import Exec.InferType (Stack (Stack), Type)
import qualified Exec.InferType as Type
import Exec.RuntimeException
import Exec.Utils (assignRet)

-- Function declarations should use the same prototype :
-- [Ast.Expr] -> IO RetVal
--
-- The first list is a list of all arguments
-- Registry is the registry
--
-- Returns RetVal

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes an Expr.Call that has been confirmed to be a builtin function
--
-- args : Expr.Call -> Registry
execBuiltin :: Type -> Stack -> IO Stack
execBuiltin (Type.Symbol "println") = printlnBuiltin
-- execBuiltin (Type.Symbol "print") = printBuiltin
-- execBuiltin (Type.Symbol "/") = divBuiltin
-- execBuiltin (Type.Symbol "%") = modulo
-- execBuiltin (Type.Symbol "*") = multiply
-- execBuiltin (Type.Symbol "-") = subBuiltin
execBuiltin (Type.Symbol "+") = add
-- execBuiltin (Type.Symbol "<") = lt
-- execBuiltin (Type.Symbol "<=") = lte
-- execBuiltin (Type.Symbol ">") = gt
-- execBuiltin (Type.Symbol ">=") = gte
-- execBuiltin (Type.Symbol "eq?") = eq
-- execBuiltin (Type.Symbol "if") = ifBuiltin
-- execBuiltin (Type.Symbol "define") = distinguishDefine
-- execBuiltin (Type.Symbol "readFile") = readFileBuiltin
-- execBuiltin (Type.Symbol "openFile") = openFileBuiltin
-- execBuiltin (Type.Symbol "head") = headBuiltin
-- execBuiltin (Type.Symbol "tail") = tailBuiltin
-- execBuiltin (Type.Symbol "init") = initBuiltin
-- execBuiltin (Type.Symbol "last") = lastBuiltin
-- execBuiltin (Type.Symbol "join") = joinBuiltin
-- execBuiltin (Type.Symbol "read") = readBuiltin
-- execBuiltin (Type.Symbol "readInt") = readIntBuiltin
execBuiltin _ = return $ throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printlnBuiltin :: Stack -> IO Stack
printlnBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "println"
printlnBuiltin (Stack callStack argStack reg) = print arg >> return newStack
    where
        ([arg], rest) = splitAt 1 argStack
        newRegistry = assignRet Type.Null reg
        newStack = Stack callStack rest newRegistry

-- printBuiltin :: [Ast.Expr] -> IO RetVal
-- printBuiltin (Ast.Literal x : ls ) = putStr x >> return (RetVal Ast.Null)
-- printBuiltin ls | not $ null ls = putStr (show $ head ls) >> return (RetVal Ast.Null)
-- printBuiltin _ _ = throwIO $ InvalidArgumentCount "println"

-- divBuiltin :: [Ast.Expr] -> IO RetVal
-- divBuiltin [Ast.Num a, Ast.Num 0] = throwIO NullDivision
-- divBuiltin [Ast.Num a, Ast.Flt 0.0] = throwIO NullDivision
-- divBuiltin [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Num (div a b)
-- divBuiltin [Ast.Flt a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((/) a b)
-- divBuiltin [Ast.Num a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((/) (fromIntegral a) b)
-- divBuiltin [Ast.Flt a, Ast.Num b] = return $ RetVal $ Ast.Flt ((/) a (fromIntegral b))
-- divBuiltin [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- divBuiltin [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- divBuiltin _ _ = throwIO $ InvalidArgumentCount "/"

-- modulo :: [Ast.Expr] -> IO RetVal
-- modulo [Ast.Num a, Ast.Num 0] = throwIO NullDivision
-- modulo [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Num (mod a b)
-- modulo [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- modulo [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- modulo _ _ = throwIO $ InvalidArgumentCount "%"

-- multiply :: [Ast.Expr] -> IO RetVal
-- multiply [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Num ((*) a b)
-- multiply [Ast.Flt a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((*) a b)
-- multiply [Ast.Num a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((*) (fromIntegral a) b)
-- multiply [Ast.Flt a, Ast.Num b] = return $ RetVal $ Ast.Flt ((*) a (fromIntegral b))
-- multiply [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- multiply [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- multiply _ _ = throwIO $ InvalidArgumentCount "*"

-- subBuiltin :: [Ast.Expr] -> IO RetVal
-- subBuiltin [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Num ((-) a b)
-- subBuiltin [Ast.Flt a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((-) a b)
-- subBuiltin [Ast.Num a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((-) (fromIntegral a) b)
-- subBuiltin [Ast.Flt a, Ast.Num b] = return $ RetVal $ Ast.Flt ((-) a (fromIntegral b))
-- subBuiltin [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- subBuiltin [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- subBuiltin _ _ = throwIO $ InvalidArgumentCount "-"

add :: Stack -> IO Stack
add (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "add"
add (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (a + b) reg

-- add :: [Ast.Expr] -> IO RetVal
-- add [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Num ((+) a b)
-- add [Ast.Flt a, Ast.Flt b] = return $ RetVal $ Ast.Flt ((+) a b)
-- add [Ast.Num a, Ast.Flt b] = return $ RetVal $ Ast.Flt (fromIntegral a + b)
-- add [Ast.Flt a, Ast.Num b] = return $ RetVal $ Ast.Flt (a + fromIntegral b)
-- add [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- add [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- add arg _ = throwIO (InvalidArgumentCount "+")

-- lt :: [Ast.Expr] -> IO RetVal
-- lt [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Boolean ((<) a b)
-- lt [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- lt [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- lt _ _ = throwIO $ InvalidArgumentCount "<"

-- lte :: [Ast.Expr] -> IO RetVal
-- lte [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Boolean ((<=) a b)
-- lte [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- lte [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- lte _ _ = throwIO $ InvalidArgumentCount "<="

-- gt :: [Ast.Expr] -> IO RetVal
-- gt [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Boolean ((>) a b)
-- gt [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- gt [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- gt _ _ = throwIO $ InvalidArgumentCount ">"

-- gte :: [Ast.Expr] -> IO RetVal
-- gte [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Boolean ((>=) a b)
-- gte [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- gte [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
-- gte _ _ = throwIO $ InvalidArgumentCount ">="

-- eq :: [Ast.Expr] -> IO RetVal
-- eq [Ast.Num a, Ast.Num b] = return $ RetVal $ Ast.Boolean ((==) a b)
-- eq [Ast.Boolean a, Ast.Boolean b] = return $ RetVal $ Ast.Boolean ((==) a b)
-- eq [a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
-- eq _ _ = throwIO $ InvalidArgumentCount "eq?"

-- readFileBuiltin :: [Ast.Expr] -> IO RetVal
-- readFileBuiltin [Ast.Handle handle] = do
--     ret <- hGetContents handle
--     return $ RetVal  (Ast.Literal ret)
-- readFileBuiltin [a] _ = throwIO $ InvalidArgument 0 (getTypeName a) (getTypeName Ast.Num)
-- readFileBuiltin _ _ = throwIO $ InvalidArgumentCount "readFile"

-- openFileBuiltin :: [Ast.Expr] -> IO RetVal
-- openFileBuiltin [Ast.Literal filePath] = do
--     handle <- GHC.IO.Handle.FD.openFile filePath ReadWriteMode
--     return $ RetVal (Ast.Handle handle)
-- openFileBuiltin [a] _ = throwIO $ InvalidArgument 0 (getTypeName a) (getTypeName Ast.Literal)
-- openFileBuiltin _ _ = throwIO $ InvalidArgumentCount "openFile"

-- ifBuiltin :: [Ast.Expr] -> IO RetVal
-- ifBuiltin [Ast.Boolean cond, caseT, caseF] reg
--     | cond = return $ RetVal caseT
--     | otherwise = return $ RetVal caseF
-- -- ifBuiltin [Ast.Boolean a, Ast.Boolean b] = return $ RetVal $ Ast.Boolean ((==) a b)
-- ifBuiltin (x:xs) _ = throwIO $ InvalidArgument 1 (getTypeName Ast.Boolean) (getTypeName x)
-- ifBuiltin _ _ = throwIO $ InvalidArgumentCount "if"

-- headBuiltin :: [Ast.Expr] -> IO RetVal
-- headBuiltin (Ast.ExprList (elem : _) : _) = return $ RetVal elem
-- headBuiltin (Ast.Literal (elem : _) : _) = return $ RetVal $ Ast.Literal [elem]
-- headBuiltin _ _ = throwIO $ InvalidArgumentCount "head"

-- tailBuiltin :: [Ast.Expr] -> IO RetVal
-- tailBuiltin (Ast.ExprList (_ : elems) : _) = return $ RetVal (Ast.ExprList elems)
-- tailBuiltin (Ast.Literal (_ : elems) : _) = return $ RetVal $ Ast.Literal elems
-- tailBuiltin _ _ = throwIO $ InvalidArgumentCount "tail"

-- initBuiltin :: [Ast.Expr] -> IO RetVal
-- initBuiltin (Ast.ExprList elems : _) = return $ RetVal (Ast.ExprList $ init elems)
-- initBuiltin (Ast.Literal elems : _) = return $ RetVal $ Ast.Literal $ init elems
-- initBuiltin _ _ = throwIO $ InvalidArgumentCount "init"

-- lastBuiltin :: [Ast.Expr] -> IO RetVal
-- lastBuiltin (Ast.ExprList elems : _) = return $ RetVal $ last elems
-- lastBuiltin (Ast.Literal elems : _) = return $ RetVal $ Ast.Literal [last elems]
-- lastBuiltin _ _ = throwIO $ InvalidArgumentCount "last"

-- joinBuiltin :: [Ast.Expr] -> IO RetVal
-- joinBuiltin (Ast.ExprList a : Ast.ExprList b : _) = return $ RetVal $ Ast.ExprList (a ++ b)
-- joinBuiltin (Ast.Literal a : Ast.Literal b : _) = return $ RetVal $ Ast.Literal (a ++ b)
-- joinBuiltin _ _ = throwIO $ InvalidArgumentCount "join"

-- readBuiltin :: [Ast.Expr] -> IO RetVal
-- readBuiltin (Ast.Literal a : _) reg
--   =  do
--     hSetBuffering stdout NoBuffering
--     putStr a
--     res <- RetVal . Ast.Literal <$> getLine
--     hSetBuffering stdout LineBuffering
--     return res
-- readBuiltin _ = do RetVal . Ast.Literal <$> getLine


-- toInt :: String -> IO Ast.Expr
-- toInt str | isNumeric str = return $ parseNum str
-- toInt _ = throwIO $ InvalidArgumentCount "readInt"


-- readIntBuiltin :: [Ast.Expr] -> IO RetVal
-- readIntBuiltin (Ast.Literal a : _) reg
--   =  do
--     hSetBuffering stdout NoBuffering
--     putStr a
--     line <- getLine
--     res <- RetVal <$> toInt line
--     hSetBuffering stdout LineBuffering
--     return res
-- readIntBuiltin _ = do
--         line <- getLine
--         RetVal <$> toInt line


-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
-- This will most likely have to be moved
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf

