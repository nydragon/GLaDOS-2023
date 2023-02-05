module Main where

-- Our modules

import Data.Maybe (fromMaybe)
import Exec
import Exec.Eval
import Exec.InteractivePrompt
import Parsing
import Parsing.Args
import Parsing.Ast
import Parsing.Cpt
import Parsing.Token
import System.Console.GetOpt
import System.Environment
import System.Exit

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
  -- Run
  -- run ast

  return ()

main :: IO ()
main =
  do
    -- Parsing arguments
    (res, fls) <- getArgs >>= parse

    let fileName = getFileName fls (file res)
    if (==) fileName "stdin"
      then interactiveMode -- interactive
      else runFile fileName -- normal
