module Main where

-- Our modules

import Data.Maybe (fromMaybe)
import Exec ()
import Exec.Eval (eval)
import Exec.Registry (Registry, RetVal (RetVal), emptyRegistry)
import Parsing ()
import Parsing.Args (Args (file), parse)
import Parsing.Ast (Expr, parseExprList)
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenizeFile)
import System.Console.GetOpt ()
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Exec.InteractivePrompt

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x : xs) b = x

runFile :: String -> IO ()
runFile filename = do
    -- Tokenize
    tokens <- tokenizeFile filename

    -- Parse CPT
    let cpt = parseTokenList tokens

    -- Parse AST
    let ast = parseExprList cpt

    let reg = emptyRegistry

    evalTree ast reg

    return ()

evalTree :: [Expr] -> Registry -> IO String
evalTree [x] reg = do
    (RetVal reg v) <- eval x reg
    return (show v)
evalTree (x : xs) reg = do
    (RetVal reg v) <- eval x reg
    evalTree xs reg

main :: IO ()
main = do
  -- Parsing arguments
  (res, fls) <- getArgs >>= parse

  let fileName = getFileName fls (file res)
  if (==) fileName "stdin"
    then interactiveMode -- interactive
    else runFile fileName -- normal
  return ()
