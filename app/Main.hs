module Main where

-- Our modules
import Parsing
import Parsing.Lang
import Parsing.Args

main :: IO ()
main = do
    -- Parse args
    parsedArgs <- parseArgs

    tokenizedcode <- tokenizeFile "./TestFiles/1"
    print tokenizedcode
    
    
