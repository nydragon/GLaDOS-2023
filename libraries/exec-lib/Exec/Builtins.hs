module Exec.Builtins where

import Control.Exception (throwIO)
import System.IO ( IOMode(ReadWriteMode), hGetContents )
import GHC.IO.Handle.FD ( openFile )
import Exec.InferType (Stack (Stack), Type)
import qualified Exec.InferType as Type
import Exec.RuntimeException
    ( RuntimeException(InvalidArgumentCount, UndefinedBehaviour, FatalError) )
import Exec.Utils (assignRet, parseNum)
import Utils (isNumeric)

-- ─── Builtin Execution ───────────────────────────────────────────────────────────────────────────

-- Executes a builtin function
execBuiltin :: Type -> Stack -> IO Stack
execBuiltin (Type.Symbol "println") = printlnBuiltin
execBuiltin (Type.Symbol "print") = printBuiltin
execBuiltin (Type.Symbol "/") = divBuiltin
execBuiltin (Type.Symbol "*") = mulBuiltin
execBuiltin (Type.Symbol "-") = subBuiltin
execBuiltin (Type.Symbol "+") = addBuiltin
execBuiltin (Type.Symbol "<") = ltBuiltin
execBuiltin (Type.Symbol "<=") = lteBuiltin
execBuiltin (Type.Symbol ">") = gtBuiltin
execBuiltin (Type.Symbol ">=") = gteBuiltin
execBuiltin (Type.Symbol "==") = eqBuiltin
execBuiltin (Type.Symbol "/=") = neqBuiltin
execBuiltin (Type.Symbol "readFile") = readFileBuiltin
execBuiltin (Type.Symbol "head") = headBuiltin
execBuiltin (Type.Symbol "tail") = tailBuiltin
execBuiltin (Type.Symbol "init") = initBuiltin
execBuiltin (Type.Symbol "last") = lastBuiltin
execBuiltin (Type.Symbol "join") = joinBuiltin
execBuiltin (Type.Symbol "read") = readBuiltin
execBuiltin (Type.Symbol "readInt") = readIntBuiltin
execBuiltin _ = return $ throwIO UndefinedBehaviour -- Builtin not found

-- ─── Builtin Implementations ─────────────────────────────────────────────────────────────────────

printlnBuiltin :: Stack -> IO Stack
printlnBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "println"
printlnBuiltin (Stack callStack as reg) = print a >> return newStack
    where
        ([a], rest) = splitAt 1 as
        newRegistry = assignRet Type.Null reg
        newStack = Stack callStack rest newRegistry

printBuiltin :: Stack -> IO Stack
printBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "print"
printBuiltin (Stack callStack as reg) = putStr (show a) >> return newStack
    where
        ([a], rest) = splitAt 1 as
        newRegistry = assignRet Type.Null reg
        newStack = Stack callStack rest newRegistry

divBuiltin :: Stack -> IO Stack
divBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "/"
divBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (a / b) reg
divBuiltin _ = throwIO $ FatalError ""

mulBuiltin :: Stack -> IO Stack
mulBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "*"
mulBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (a * b) reg
mulBuiltin _ = throwIO $ FatalError ""

subBuiltin :: Stack -> IO Stack
subBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "-"
subBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (a - b) reg
subBuiltin _ = throwIO $ FatalError ""

addBuiltin :: Stack -> IO Stack
addBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "+"
addBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (a + b) reg
addBuiltin _ = throwIO $ FatalError ""

ltBuiltin :: Stack -> IO Stack
ltBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "<"
ltBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a < b) reg
ltBuiltin _ = throwIO $ FatalError ""

lteBuiltin :: Stack -> IO Stack
lteBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "<="
lteBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a <= b) reg
lteBuiltin _ = throwIO $ FatalError ""

gtBuiltin :: Stack -> IO Stack
gtBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount ">"
gtBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a > b) reg
gtBuiltin _ = throwIO $ FatalError ""

gteBuiltin :: Stack -> IO Stack
gteBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount ">="
gteBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a >= b) reg
gteBuiltin _ = throwIO $ FatalError ""

eqBuiltin :: Stack -> IO Stack
eqBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "=="
eqBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a == b) reg
eqBuiltin _ = throwIO $ FatalError ""

neqBuiltin :: Stack -> IO Stack
neqBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "/="
neqBuiltin (Stack callStack (a : b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.Boolean $ a /= b) reg
neqBuiltin _ = throwIO $ FatalError ""

readFileBuiltin :: Stack -> IO Stack
readFileBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "readFile"
readFileBuiltin (Stack callStack (Type.String a : newArgStack) reg) = do
    file <- openFile a ReadWriteMode >>= hGetContents
    return  $ Stack callStack newArgStack $ assignRet (Type.String file) reg
readFileBuiltin _ = throwIO $ FatalError ""

headBuiltin :: Stack -> IO Stack
headBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "head"
headBuiltin (Stack callStack (Type.List a : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (head a) reg
headBuiltin _ = throwIO $ FatalError ""

tailBuiltin :: Stack -> IO Stack
tailBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "tail"
tailBuiltin (Stack callStack (Type.List a : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.List $ tail a) reg
tailBuiltin _ = throwIO $ FatalError ""

initBuiltin :: Stack -> IO Stack
initBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "init"
initBuiltin (Stack callStack (Type.List a : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.List $ init a) reg
initBuiltin _ = throwIO $ FatalError ""

lastBuiltin :: Stack -> IO Stack
lastBuiltin (Stack _ argStack _) | null argStack = throwIO $ InvalidArgumentCount "last"
lastBuiltin (Stack callStack (Type.List a : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (last a) reg
lastBuiltin _ = throwIO $ FatalError ""

joinBuiltin :: Stack -> IO Stack
joinBuiltin (Stack _ argStack _) | length argStack < 2 = throwIO $ InvalidArgumentCount "join"
joinBuiltin (Stack callStack (Type.List a : Type.List b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.List $ a ++ b) reg
joinBuiltin (Stack callStack (Type.String a : Type.String b : newArgStack) reg) = return newStack
    where newStack = Stack callStack newArgStack $ assignRet (Type.String $ a ++ b) reg
joinBuiltin _ = throwIO $ FatalError ""

readBuiltin :: Stack -> IO Stack
readBuiltin (Stack callStack argStack reg) = do
    res <- Type.String <$> getLine
    return $ Stack callStack argStack $ assignRet res reg

toInt :: String -> Type
toInt str | isNumeric str = parseNum str
toInt _ = Type.Null


readIntBuiltin :: Stack -> IO Stack
readIntBuiltin (Stack callStack argStack reg) = do
    line <- getLine
    return $ Stack callStack argStack $ assignRet (toInt line) reg

