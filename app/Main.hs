module Main where

import Parsing

main :: IO ()
main = do
    let tokenizedcode = tokenize "(define x 2)\n\n( add    x 3 )"
    print tokenizedcode
    
    
