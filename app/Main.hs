module Main where

-- Library modules
import System.Args

-- Our modules
import Parsing

main :: IO ()
main = do
    let tokenizedcode = tokenizePath "(define x 2)\n\n( add    x 3 )"
    print tokenizedcode
    
    
