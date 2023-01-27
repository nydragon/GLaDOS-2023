module Main where

-- Our modules
import Parsing
import Parsing.Lang
import Parsing.Args

main :: IO ()
main = do
    -- Parse args
    parsedArgs <- parseArgs

    -- For the time being since we don't know how to pass args with cabal
    -- we use tokenizeFile immediately
    tokenizedcode <- tokenizeFile "./TestFiles/2"
    
    print (parseTokenList tokenizedcode)
