module Main where

-- Our modules
import Parsing
import Parsing.Lang
import Parsing.Args

main :: IO ()
main = do
    parsedArgs <- parseArgs
    print parsedArgs
    -- let tokenizedcode = tokenizePath "./TestFiles/1"
    -- print tokenizedcode
    
    
