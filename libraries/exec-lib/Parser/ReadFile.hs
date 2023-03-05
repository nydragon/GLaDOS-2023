module ReadFile where

import Instruction (Instruction (Pop))
import Data.List (isPrefixOf)

stringToInstruction :: String -> Instruction 
stringToInstruction str | "pop" `isPrefixOf` str = Pop $ take 4 str


splitOn :: Char -> String -> [String]
splitOn del str = substring : splitOn del res
    where (substring, _ : res) = break (== del) str

parseFile :: FilePath -> IO [String] 
parseFile a = readFile a >>= \contents -> return $ splitOn '\n' contents