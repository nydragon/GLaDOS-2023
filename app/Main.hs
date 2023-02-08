module Main where

-- Our modules

import Data.Maybe (fromMaybe)
import Exec ()
import Exec.Eval (eval)
import Exec.Registry (Registry, RetVal (RetVal), emptyRegistry)
import Parsing ()
import Parsing.Args (Args (file, interactive), parse)
import Parsing.Ast (Expr, parseExprList)
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenizeFile, Token, tokenize)
import System.Console.GetOpt ()
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Exec.InteractivePrompt ( interactiveMode )
import GHC.IO.Handle
import GHC.IO.FD

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x : xs) b = x

runFile :: String -> IO ()
runFile filename = do
    -- Tokenize
    tokens <- tokenizeFile filename
    execute tokens

execute :: [Token] -> IO ()
execute tokens = do
     -- Parse CPT
    let cpt = parseTokenList tokens

    -- Parse AST
    let ast = parseExprList cpt

    let reg = emptyRegistry

    evalTree ast reg


evalTree :: [Expr] -> Registry -> IO ()
evalTree [x] reg = do
    eval x reg
    return ()
evalTree (x : xs) reg = do
    (RetVal reg v) <- eval x reg
    evalTree xs reg


main :: IO ()
main = do
    -- Parsing arguments
    (res, fls) <- getArgs >>= parse

    let fileName = getFileName fls (file res)

    if interactive res
    then interactiveMode -- interactive
    else if fileName == "stdin"
        then do 
            file <- getContents
            execute $ tokenize file
        else runFile fileName -- normal
    return ()
