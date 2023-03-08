module Main where

import FunctionBlock
import Utils
import Parsing.Ast
import Parsing.Cpt
import Parsing.Token
import Compilation.Registry
import Compilation.Compile
import Exec
import Exec.InferType

mainLoop :: [FunctionBlock] -> Registry -> IO ()
mainLoop fns reg = do
    -- Get input and parse into AST
    ast <- parseExprList . parseTokenList . tokenize <$> getInput [] 0

    -- Remove main from functions
    let withoutMain = removeMainFunc fns

    -- Compile new functions
    let (compiled, newReg) = compileProgramAddition (ExprList ast) reg
    let resPgm = head compiled : withoutMain ++ init compiled

    -- Run assembly language
    _ <- executeFunc resPgm "main" initStack

    -- Recursive Call
    mainLoop resPgm reg

main :: IO ()
main = do
    putStrLn "*** Welcome to the glados interactive prompt ***"

    mainLoop [] emptyRegistry