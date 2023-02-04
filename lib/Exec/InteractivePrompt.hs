import System.IO
import Data.List
import System.Exit

--Function that counts the character received as argument
-- Returns the number of times this character appears in a string
countChar :: Char -> String -> Int
countChar char = length . filter (== char)

--Function that checks the occurance between '(' and ')'.
-- Returns a boolean
occurenceBrackets :: Int -> Int -> Bool
occurenceBrackets num_open num_close = num_open == num_close && num_open /= 0

--Recursive function that receives input
-- Reads input, triggers program exit, terminates after good format, recursive if bad
checkBrackets :: [String] -> IO [String]
checkBrackets inputs = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if input == "\ESC" then
    exitSuccess
  else do
    let num_open = countChar '(' input
    let num_close = countChar ')' input
    if occurenceBrackets num_open num_close
      then return inputs
      else do
        putStrLn "Not ok. Enter again."
        checkBrackets (input:inputs)

--Fonction main
--  Affiche entrée "input" au bon format
main :: IO ()
main = do
  putStrLn "*** BONUS GLADOS ***"
  inputs <- checkBrackets []
  print inputs
  --get the return of the "checkBrackets" function
  --which gives the correct test format with true occurance '(' == ')'
  