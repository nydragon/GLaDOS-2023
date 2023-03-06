module Parser.ReadFile where

import Instruction (Instruction (Pop, Push, Call, Init, Move))
import Data.List (isPrefixOf)
import FunctionBlock (FunctionBlock (Function))

split' :: String -> String -> (String, String)
split' lStr (' ':xs)  = (lStr,xs)
split' lStr (x:xs) = split' xs (lStr ++ [x])

split :: String -> (String, String)
split = split' ""

stringToInstruction :: String -> Instruction 
stringToInstruction str | "push" `isPrefixOf` str = Push $ drop 5 str
stringToInstruction str | "pop"  `isPrefixOf` str = Pop  $ drop 4 str
stringToInstruction str | "call" `isPrefixOf` str = Call $ drop 5 str
stringToInstruction str | "init" `isPrefixOf` str = Init $ drop 5 str
stringToInstruction str | "move" `isPrefixOf` str = uncurry Move (split str)

convertToInstructions :: [String] -> [FunctionBlock]
convertToInstructions [] = []
convertToInstructions (l:ls)
    | "func" `isPrefixOf` l && not (null rest) = Function name funcBody : convertToInstructions (tail rest)
    | "func" `isPrefixOf` l = [Function name funcBody]
    | otherwise = convertToInstructions ls
    where
        (funcSection,rest) = break (== "end") ls
        name = drop 5 l
        funcBody = map stringToInstruction funcSection

splitOn :: Char -> String -> [String]
splitOn del [] = []
splitOn del str 
    | not $ null rest = substring : splitOn del (tail rest)
    | otherwise = [substring]
    where
        (substring, rest) = break (== del) str

parseFile :: FilePath -> IO [String] 
parseFile a = readFile a >>= \contents -> return $ splitOn '\n' contents