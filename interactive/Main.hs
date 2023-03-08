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
import Exec.Utils (lookupRet, assignRet)
import Compilation.Utils (isAtomic)
import qualified Exec.InferType as Type
import Data.Map (delete)


printRet :: Maybe Type -> IO ()
printRet (Just t) = print t
printRet _ = return ()

mainLoop :: [FunctionBlock] -> Registry -> Stack -> IO ()
mainLoop fns reg stack = do
    -- Get input and parse into AST
    ast <- parseExprList . parseTokenList . tokenize <$> getInput [] 0
    
    -- Remove main from functions
    let withoutMain = removeMainFunc fns

    -- Compile new functions
    let (compiled, newReg) = compileProgramAddition (ExprList ast) reg

    let resPgm = compiled ++ withoutMain

    -- Run assembly language
    (Stack _ _ runtimeExpr) <- executeFunc resPgm "main" stack

    printRet $ lookupRet runtimeExpr
    let cleanedReg = delete (Type.Symbol "#RET") runtimeExpr

    -- Recursive Call
    mainLoop resPgm newReg $ Stack [] [] cleanedReg

main :: IO ()
main = do
    putStrLn "*** Welcome to the glados interactive prompt ***"

    mainLoop [] emptyRegistry initStack