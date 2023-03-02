module Exec.Builtins where

import Control.Exception (throwIO)
import Data.Typeable ( Typeable, typeOf )
import Exec.Registry ( RetVal(..), Registry )
import Exec.RuntimeException
    ( RuntimeException(InvalidArgumentCount, NotYetImplemented,
                       UndefinedBehaviour, NullDivision, InvalidArgument) )
import qualified Parsing.Ast as Ast
import Exec.Variables ( defineVar )
import Exec.Function ( defineFunc )
import Debug.Trace ()
import GHC.IO.FD ()
import System.IO
    ( hGetContents,
      IOMode(ReadWriteMode),
      hSetBuffering,
      BufferMode(LineBuffering, NoBuffering),
      stdout )
import GHC.IO.Handle.FD ( openFile )
import Foreign.C.Types ()
import Exec.Utils ( convert, isNumeric, parseNum )
import GHC.Float
import Data.Char (isDigit)

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
    "/" -> divBuiltin ls reg
    "%" -> modulo ls reg
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
    "readFile" -> readFileBuiltin ls reg
    "openFile" -> openFileBuiltin ls reg
    "head" -> headBuiltin ls reg
    "tail" -> tailBuiltin ls reg
    "init" -> initBuiltin ls reg
    "last" -> lastBuiltin ls reg
    "join" -> joinBuiltin ls reg
    "read" -> readBuiltin ls reg
    "readInt" -> readIntBuiltin ls reg
    _ -> throwIO NotYetImplemented
execBuiltin _ _ = throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printlnBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
printlnBuiltin (Ast.Literal x : ls ) reg = putStrLn x >> return (RetVal reg Ast.Null)
printlnBuiltin ls reg | not $ null ls = print (head ls) >> return (RetVal reg Ast.Null)
printlnBuiltin _ _ = throwIO $ InvalidArgumentCount "println"

printBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
printBuiltin (Ast.Literal x : ls ) reg = putStr x >> return (RetVal reg Ast.Null)
printBuiltin ls reg | not $ null ls = putStr (show $ head ls) >> return (RetVal reg Ast.Null)
printBuiltin _ _ = throwIO $ InvalidArgumentCount "println"

divBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
divBuiltin [Ast.Num a, Ast.Num 0] reg = throwIO NullDivision
divBuiltin [Ast.Num a, Ast.Flt 0.0] reg = throwIO NullDivision
divBuiltin [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num (div a b)
divBuiltin [Ast.Flt a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((/) a b)
divBuiltin [Ast.Num a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((/) (fromIntegral a) b)
divBuiltin [Ast.Flt a, Ast.Num b] reg = return $ RetVal reg $ Ast.Flt ((/) a (fromIntegral b))
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
multiply [Ast.Flt a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((*) a b)
multiply [Ast.Num a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((*) (fromIntegral a) b)
multiply [Ast.Flt a, Ast.Num b] reg = return $ RetVal reg $ Ast.Flt ((*) a (fromIntegral b))
multiply [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
multiply [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
multiply _ _ = throwIO $ InvalidArgumentCount "*"

subBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
subBuiltin [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num ((-) a b)
subBuiltin [Ast.Flt a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((-) a b)
subBuiltin [Ast.Num a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((-) (fromIntegral a) b)
subBuiltin [Ast.Flt a, Ast.Num b] reg = return $ RetVal reg $ Ast.Flt ((-) a (fromIntegral b))
subBuiltin [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
subBuiltin [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
subBuiltin _ _ = throwIO $ InvalidArgumentCount "-"

add :: [Ast.Expr] -> Registry -> IO RetVal
add [Ast.Num a, Ast.Num b] reg = return $ RetVal reg $ Ast.Num ((+) a b)
add [Ast.Flt a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt ((+) a b)
add [Ast.Num a, Ast.Flt b] reg = return $ RetVal reg $ Ast.Flt (fromIntegral a + b)
add [Ast.Flt a, Ast.Num b] reg = return $ RetVal reg $ Ast.Flt (a + fromIntegral b)
add [Ast.Num a, b] _ = throwIO $ InvalidArgument 1 (getTypeName a) (getTypeName b)
add [a, Ast.Num b] _ = throwIO $ InvalidArgument 0 (getTypeName b) (getTypeName a)
add arg _ = throwIO (InvalidArgumentCount "+")

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
eq _ _ = throwIO $ InvalidArgumentCount "eq?"

readFileBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
readFileBuiltin [Ast.Handle handle] reg = do
    ret <- hGetContents handle
    return $ RetVal reg  (Ast.Literal ret)
readFileBuiltin [a] _ = throwIO $ InvalidArgument 0 (getTypeName a) (getTypeName Ast.Num)
readFileBuiltin _ _ = throwIO $ InvalidArgumentCount "readFile"

openFileBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
openFileBuiltin [Ast.Literal filePath] reg = do
    handle <- GHC.IO.Handle.FD.openFile filePath ReadWriteMode
    return $ RetVal reg (Ast.Handle handle)
openFileBuiltin [a] _ = throwIO $ InvalidArgument 0 (getTypeName a) (getTypeName Ast.Literal)
openFileBuiltin _ _ = throwIO $ InvalidArgumentCount "openFile"

ifBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
ifBuiltin [Ast.Boolean cond, caseT, caseF] reg
    | cond = return $ RetVal reg caseT
    | otherwise = return $ RetVal reg caseF
-- ifBuiltin [Ast.Boolean a, Ast.Boolean b] reg = return $ RetVal reg $ Ast.Boolean ((==) a b)
ifBuiltin (x:xs) _ = throwIO $ InvalidArgument 1 (getTypeName Ast.Boolean) (getTypeName x)
ifBuiltin _ _ = throwIO $ InvalidArgumentCount "if"

headBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
headBuiltin (Ast.ExprList (elem : _) : _) reg = return $ RetVal reg elem
headBuiltin (Ast.Literal (elem : _) : _) reg = return $ RetVal reg $ Ast.Literal [elem]
headBuiltin _ _ = throwIO $ InvalidArgumentCount "head"

tailBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
tailBuiltin (Ast.ExprList (_ : elems) : _) reg = return $ RetVal reg (Ast.ExprList elems)
tailBuiltin (Ast.Literal (_ : elems) : _) reg = return $ RetVal reg $ Ast.Literal elems
tailBuiltin _ _ = throwIO $ InvalidArgumentCount "tail"

initBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
initBuiltin (Ast.ExprList elems : _) reg = return $ RetVal reg (Ast.ExprList $ init elems)
initBuiltin (Ast.Literal elems : _) reg = return $ RetVal reg $ Ast.Literal $ init elems
initBuiltin _ _ = throwIO $ InvalidArgumentCount "init"

lastBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
lastBuiltin (Ast.ExprList elems : _) reg = return $ RetVal reg $ last elems
lastBuiltin (Ast.Literal elems : _) reg = return $ RetVal reg $ Ast.Literal [last elems]
lastBuiltin _ _ = throwIO $ InvalidArgumentCount "last"

joinBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
joinBuiltin (Ast.ExprList a : Ast.ExprList b : _) reg = return $ RetVal reg $ Ast.ExprList (a ++ b)
joinBuiltin (Ast.Literal a : Ast.Literal b : _) reg = return $ RetVal reg $ Ast.Literal (a ++ b)
joinBuiltin _ _ = throwIO $ InvalidArgumentCount "join"

readBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
readBuiltin (Ast.Literal a : _) reg
  =  do
    hSetBuffering stdout NoBuffering
    putStr a
    res <- RetVal reg . Ast.Literal <$> getLine
    hSetBuffering stdout LineBuffering
    return res
readBuiltin _ reg = do RetVal reg . Ast.Literal <$> getLine


toInt :: String -> IO Ast.Expr
toInt str | isNumeric str = return $ parseNum str
toInt _ = throwIO $ InvalidArgumentCount "readInt"


readIntBuiltin :: [Ast.Expr] -> Registry -> IO RetVal
readIntBuiltin (Ast.Literal a : _) reg
  =  do
    hSetBuffering stdout NoBuffering
    putStr a
    line <- getLine
    res <- RetVal reg <$> toInt line
    hSetBuffering stdout LineBuffering
    return res
readIntBuiltin _ reg = do
        line <- getLine
        RetVal reg <$> toInt line


-- ─── Utilities ───────────────────────────────────────────────────────────────────────────────────

-- Get string representation of type name
-- This will most likely have to be moved
getTypeName :: Typeable a => a -> String
getTypeName = show . typeOf

-- Utility function to distinguish between variable definition and function definition
-- Args : [Expr] of args -> Registry
distinguishDefine :: [Ast.Expr] -> Registry -> IO RetVal
distinguishDefine [Ast.Symbole s, Ast.Call "lambda" [Ast.ExprList args, body]] reg = defineFunc [Ast.Symbole s, Ast.Call "lambda" [Ast.ExprList args, body]] reg
distinguishDefine args reg = defineVar args reg
