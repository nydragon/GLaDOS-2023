module Utils where

import System.IO
import Data.List
import System.Exit

-- Function that counts the character received as argument
-- Returns the number of times this character appears in a string
countChar :: Char -> String -> Int
countChar char = length . filter (== char)

printprompt :: Int -> IO ()
printprompt 0 = putStr "> " >> hFlush stdout
printprompt _ = putStr "  " >> hFlush stdout

recurse :: [String] -> Int -> Int -> IO String
recurse _ 0 _ = print "Open with a bracket." >> getInput [] 0
recurse inputs _ _
  | "\ESC" `isInfixOf` last inputs = exitSuccess
recurse inputs open close
  | open == close = return $ concat inputs
  | otherwise = getInput inputs (open - close)

-- Recursive function that receives input
-- Reads input, triggers program exit, terminates after good format, recursive if bad
getInput :: [String] -> Int -> IO String
getInput inputs openBrackets = do
    -- Get Input
    printprompt openBrackets
    input <- getLine

    let num_open = countChar '(' input
    let num_close = countChar ')' input

    recurse (inputs <> [input ++ "\n"]) (openBrackets + num_open) num_close