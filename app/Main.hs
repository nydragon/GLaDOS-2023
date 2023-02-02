module Main where

-- Our modules
import Parsing
import Parsing.Token
import Parsing.Cpt
import Parsing.Args
import System.Console.GetOpt

import System.Environment
import System.Exit
import Data.Maybe (fromMaybe)

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x:xs) b = x

main :: IO ()
main = do
    -- Parsing arguments
    (res, fls) <- getArgs >>= parse

    let fileName = getFileName fls (file res)
    print ("Execute file: " ++  fileName)
    if (==) fileName "stdin"
        then exitSuccess -- interactive
        else exitSuccess -- normal