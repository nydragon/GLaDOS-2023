module Main where

-- Our modules
import Parsing
import Parsing.Token
import Parsing.Cpt
import Parsing.Args
import System.Console.GetOpt

import System.Environment
import System.Exit

main :: IO ()
main = do
    -- Parse args
    (res,fs) <- getArgs >>= parse
    print res
    -- print files
    -- For the time being since we don't know how to pass args with cabal
    -- we use tokenizeFile immediately
