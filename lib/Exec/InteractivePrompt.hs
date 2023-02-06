module Exec.InteractivePrompt where

import Data.List ()
import Exec.Eval
import Exec.Registry
import Parsing.Ast (parseExprList)
import qualified Parsing.Ast as Ast
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenize)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

-- Function that counts the character received as argument
-- Returns the number of times this character appears in a string
countChar :: Char -> String -> Int
countChar char = length . filter (== char)

-- Function that checks the occurance between '(' and ')'.
-- Returns a boolean
occurenceBrackets :: Int -> Int -> Bool
occurenceBrackets num_open num_close = num_open == num_close && num_open /= 0

recurse :: [String] -> Int -> Int -> IO String
recurse inputs 0 close = print "Open with a bracket." >> getInput [] 0
recurse inputs open close
  | open == close = return $ concat inputs
  | otherwise = getInput inputs (open - close)

-- Recursive function that receives input
-- Reads input, triggers program exit, terminates after good format, recursive if bad
getInput :: [String] -> Int -> IO String
getInput inputs openBrackets = do
  if openBrackets == 0
    then putStr "> "
    else putStr "  "
  hFlush stdout
  input <- getLine
  if input == "\ESC"
    then exitSuccess
    else do
      let num_open = countChar '(' input
      let num_close = countChar ')' input

      recurse (inputs <> [input ++ "\n"]) (openBrackets + num_open) num_close

loop :: Registry -> IO ()
loop reg = do
  -- Parse AST
  ast <- parseExprList . parseTokenList . tokenize <$> getInput [] 0

  -- Run
  (RetVal newReg _) <- eval (Ast.ExprList ast) reg

  loop newReg

interactiveMode :: IO ()
interactiveMode = do
  putStrLn "*** BONUS GLADOS ***"

  -- Run
  loop emptyRegistry

  return ()