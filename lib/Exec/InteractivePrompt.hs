import Data.List
import System.Exit
import System.IO

-- Function that counts the character received as argument
-- Returns the number of times this character appears in a string
countChar :: Char -> String -> Int
countChar char = length . filter (== char)

-- Function that checks the occurance between '(' and ')'.
-- Returns a boolean
occurenceBrackets :: Int -> Int -> Bool
occurenceBrackets num_open num_close = num_open == num_close && num_open /= 0

-- Recursive function that receives input
-- Reads input, triggers program exit, terminates after good format, recursive if bad
checkBrackets :: [String] -> Int -> IO String
checkBrackets inputs openBrackets = do
  if openBrackets == 0
    then putStr "> "
    else putStr "  "
  hFlush stdout
  input <- getLine
  if input == "\ESC"
    then exitSuccess
    else do
      let num_open = countChar '(' input
      if (openBrackets + num_open) == 0
        then do
          print "Open with a bracket."
          checkBrackets [] 0
        else do
          let num_close = countChar ')' input
          if (openBrackets + num_open) == num_close
            then return $ concat (inputs <> [input ++ "\n"])
            else do
              checkBrackets (inputs <> [input ++ "\n"]) ((openBrackets + num_open) - num_close)

-- Fonction main
--  Affiche entrée "input" au bon format
main :: IO ()
main = do
  putStrLn "*** BONUS GLADOS ***"
  inputs <- checkBrackets [] 0
  putStr inputs

-- get the return of the "checkBrackets" function
-- which gives the correct test format with true occurance '(' == ')'