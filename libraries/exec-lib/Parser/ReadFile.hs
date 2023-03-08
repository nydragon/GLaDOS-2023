module Parser.ReadFile where

import Instruction (Instruction (Pop, Push, Call, Init, Move, Conditional))
import Data.List (isPrefixOf)
import FunctionBlock (FunctionBlock (Function))
import Control.Exception (throw)
import Exec.RuntimeException (RuntimeException(FatalError, NotAnInstruction))

split' :: String -> String -> (String, String)
split' lStr (' ':xs)  = (lStr,xs)
split' lStr (x:xs) = split' (lStr ++ [x]) xs
split' "" "" = throw $ FatalError ""
split' (_:_) [] = throw $ FatalError ""

splitString :: String -> String -> (String, String)
splitString lStr ('"' : ' ' : xs) = (lStr ++ "\"", xs)
splitString lStr (x:xs) = splitString (lStr ++ [x]) xs
splitString _ _ = throw $ FatalError ""

split :: String -> (String, String)
split "" = throw $ FatalError ""
split str | head str == '"' = splitString "" str
split str = split' "" str

stringToInstruction :: String -> Instruction 
stringToInstruction str | "push" `isPrefixOf` str = Push $ drop 5 str
stringToInstruction str | "pop"  `isPrefixOf` str = Pop  $ drop 4 str
stringToInstruction str | "call" `isPrefixOf` str = Call $ drop 5 str
stringToInstruction str | "init" `isPrefixOf` str = Init $ drop 5 str
stringToInstruction str | "move" `isPrefixOf` str = uncurry Move (split $ drop 5 str)
stringToInstruction str = throw $ NotAnInstruction str

getCondition :: [String] -> ([Instruction], [String])
getCondition (i:is) | "if" `isPrefixOf` i = ([con], rest) 
    where (con, rest) = reduceConditional' (i:is)
getCondition ("then":is) = ([], is)
getCondition ("if":is) = (con, rest) 
    where (con, rest) = getCondition is
getCondition (i:is) = (stringToInstruction i:con, rest) 
    where (con, rest) = getCondition is
getCondition [] = throw $ FatalError ""

getLeftBranch :: [String] -> ([Instruction], [String])
getLeftBranch (i:is) | "if" `isPrefixOf` i =  ([con], rest) 
    where (con, rest) = reduceConditional' (i:is)
getLeftBranch ("else":is) = ([], is)
getLeftBranch ("then":is) = (con, rest) 
    where (con, rest) = getLeftBranch is
getLeftBranch (i:is) = (stringToInstruction i:con, rest) 
    where (con, rest) = getLeftBranch is
getLeftBranch [] = throw $ FatalError ""

getRightBranch :: [String] -> ([Instruction], [String])
getRightBranch (i:is) | "if" `isPrefixOf` i = ([con], rest) 
    where (con, rest) = reduceConditional' (i:is)
getRightBranch ("enif":is) = ([], is)
getRightBranch ("else":is) = (con, rest) 
    where (con, rest) = getRightBranch is
getRightBranch (i:is) = (stringToInstruction i:con, rest) 
    where (con, rest) = getRightBranch is
getRightBranch [] = throw $ FatalError ""

 
reduceConditional' :: [String] -> (Instruction, [String])
reduceConditional' instr = (Instruction.Conditional cond lBranch rBranch, rest3)
    where
        (cond, rest1) = getCondition (tail instr)
        (lBranch, rest2) = getLeftBranch rest1
        (rBranch, rest3) = getRightBranch rest2

reduceConditional :: [String] -> Instruction
reduceConditional instr = fst $ reduceConditional' instr

parseBody :: [String] -> [Instruction]
parseBody instr | "if" `isPrefixOf` head instr = [reduceConditional instr]
parseBody instr = map stringToInstruction instr

convertToInstructions :: [String] -> [FunctionBlock]
convertToInstructions [] = []
convertToInstructions (l:ls)
    | "func" `isPrefixOf` l && not (null rest) = Function name funcBody : convertToInstructions (tail rest)
    | "func" `isPrefixOf` l = [Function name funcBody]
    | otherwise = convertToInstructions ls
    where
        (funcSection,rest) = break (== "end") ls
        name = drop 5 l
        funcBody = parseBody funcSection

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn del str 
    | not $ null rest = substring : splitOn del (tail rest)
    | otherwise = [substring]
    where
        (substring, rest) = break (== del) str

parseFile :: FilePath -> IO [String] 
parseFile a = readFile a >>= \contents -> return $ splitOn '\n' contents