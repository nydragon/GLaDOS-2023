module Main where

-- Our modules

import Data.Maybe (fromMaybe)
import Exec ()
import Exec.Eval (eval)
import Exec.Registry (RetVal (RetVal), emptyRegistry)
import Parsing ()
import Parsing.Args (Args (file), parse)
import Parsing.Ast (parseExprList)
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenizeFile)
import System.Console.GetOpt ()
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Exec.InteractivePrompt

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x : xs) b = x

runFile :: String -> IO String
runFile filename = do
  -- Tokenize
  tokens <- tokenizeFile filename

  -- Parse CPT
  let cpt = parseTokenList tokens

  -- Parse AST
  let ast = parseExprList cpt

  let reg = emptyRegistry

  (RetVal d v) <- eval (head ast) reg

  print v
  -- Run
  -- run ast

  return (show v)

main :: IO ()
main = do
  -- Parsing arguments
  (res, fls) <- getArgs >>= parse

  let fileName = getFileName fls (file res)
  if (==) fileName "stdin"
    then interactiveMode -- interactive
    else runFile fileName -- normal
  return ()
