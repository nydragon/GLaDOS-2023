module Main where

import Parsing.Args (Args (file), parse)
import Parsing.Ast (Expr (ExprList), parseExprList)
import Parsing.Cpt (parseTokenList)
import Parsing.Token (tokenizeFile)
import Parsing.TokenType ( Token )
import Compilation.Compile (compileProgram)

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

getFileName :: [String] -> Maybe FilePath -> String
getFileName [] b = fromMaybe "stdin" b
getFileName (x : _) _ = x

compileFile :: String -> IO ()
compileFile filename = do
    -- Tokenize
    tokens <- tokenizeFile filename
    compile tokens

compile :: [Token] -> IO ()
compile tokens = do
     -- Parse CPT
    let cpt = parseTokenList tokens

    -- Parse AST
    let ast = parseExprList cpt
    print ast
    -- Compile
    let assembledProgram = compileProgram $ ExprList ast

    let content = concatMap show assembledProgram

    writeFile "a.out" content

    return ()



main :: IO ()
main = do
    -- Parsing arguments
    (res, fls) <- getArgs >>= parse

    let fileName = getFileName fls (file res)

    compileFile fileName -- normal

    return ()
