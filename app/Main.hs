module Main where

-- Our modules
import Parsing
import Parsing.Token
import Parsing.Cpt
import Parsing.Args

main :: IO ()
main = do
    -- Parse args
    parsedArgs <- parseArgs

    -- For the time being since we don't know how to pass args with cabal
    -- we use tokenizeFile immediately

    tokenizedcode <- tokenizeFile "./TestFiles/sample1.scm"

    print tokenizedcode
    print (parseTokenList tokenizedcode)
