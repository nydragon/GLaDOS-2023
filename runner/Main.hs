module Main where
import Parser.ReadFile (parseFile, stringToInstruction, convertToInstructions)
import Exec (executeFunc)
import Exec.InferType (initStack)

main :: IO ()
main = do
    file <- parseFile "test.cmp"
    let instr = convertToInstructions file

    executeFunc instr "main" initStack
    return ()