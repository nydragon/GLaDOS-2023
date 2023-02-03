module Main where

-- Our modules
import Parsing
import Parsing.Token
import Parsing.Cpt
import Parsing.Ast
import Parsing.Args
import System.Console.GetOpt
import Exec

import System.Environment
import System.Exit
import Data.Maybe (fromMaybe)

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x:xs) b = x

runFile :: String -> IO ()
runFile filename = do
    -- Tokenize
    tokens <- tokenizeFile filename

    -- Parse CPT
    let cpt = parseTokenList tokens

    -- Parse AST
    let ast = parseExprList cpt

    run ast

    return ()


main :: IO ()
main = do
    -- Parsing arguments
    (res, fls) <- getArgs >>= parse

    let fileName = getFileName fls (file res)
    print ("Execute file: " ++  fileName)
    if (==) fileName "stdin"
        then exitSuccess -- interactive
        else runFile fileName -- normal
