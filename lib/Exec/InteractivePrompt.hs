module Exec.InteractivePrompt where

import Data.List (isInfixOf)
import Exec.Eval
import Exec.Registry
import Parsing.Ast (parseExprList)
import qualified Parsing.Ast as Ast
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenize)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Exec.Utils ( isAtomic )

-- Function that counts the character received as argument
-- Returns the number of times this character appears in a string
countChar :: Char -> String -> Int
countChar char = length . filter (== char)

recurse :: [String] -> Int -> Int -> IO String
recurse inputs 0 close = print "Open with a bracket." >> getInput [] 0
recurse inputs _ _
  | "\ESC" `isInfixOf` last inputs = exitSuccess
recurse inputs open close
  | open == close = return $ concat inputs
  | otherwise = getInput inputs (open - close)

printprompt :: Int -> IO ()
printprompt 0 = putStr "> " >> hFlush stdout
printprompt _ = putStr "  " >> hFlush stdout

-- Recursive function that receives input
-- Reads input, triggers program exit, terminates after good format, recursive if bad
getInput :: [String] -> Int -> IO String
getInput inputs openBrackets = do
  printprompt openBrackets
  input <- getLine
  let num_open = countChar '(' input
  let num_close = countChar ')' input
  recurse (inputs <> [input ++ "\n"]) (openBrackets + num_open) num_close

display :: Ast.Expr -> IO ()
display (Ast.ExprList (a : as)) | isAtomic a && a /= Ast.Null = print a
display _ = return ()

loop :: Registry -> IO ()
loop reg = do
  -- Parse AST
  ast <- parseExprList . parseTokenList . tokenize <$> getInput [] 0

  -- Run
  (RetVal newReg res) <- eval (Ast.ExprList ast) reg

  display res

  loop newReg

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "*** BONUS GLADOS ***"

  -- Run
  loop emptyRegistry

  return ()