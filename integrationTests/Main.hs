module Main where

import System.Directory
import System.FilePath
import System.Process
import System.Console.ANSI
import System.Exit
import Data.List

main :: IO()
main = loop =<< getFiles

getFiles :: IO[String]
getFiles = map takeBaseName.filter(`notElem` [".", ".."]) <$> getDirectoryContents "./IntegrationTestFolder/TestFiles/"

printOk :: IO()
printOk = setSGR [SetColor Foreground Vivid Green] >>
    putStrLn ("OK") >> setSGR [Reset]

printError :: IO()
printError = setSGR [SetColor Foreground Vivid Red] >>
    putStrLn ("Error") >> setSGR [Reset]

printRes :: Bool -> Bool -> ExitCode -> IO()
printRes True False ExitSuccess =
    putStr ("\tOutPut: ") >> printOk >>
    putStr ("\tExit Status: ") >> printOk
printRes True False (ExitFailure n) =
    putStr ("\tOutPut: ") >> printOk >>
    putStr ("\tExit Status: ") >> printError
printRes True True ExitSuccess =
    putStr ("\tOutPut: ") >> printOk >>
    putStr("\tExit Status: ") >> printError
printRes True True (ExitFailure n) =
    putStr ("\tOutPut: ") >> printOk >>
    putStr("\tExit Status: ") >> printOk
printRes False False ExitSuccess =
    putStr ("\tOutPut: ") >> printError >>
    putStr ("\tExit Status: ") >> printOk
printRes False False (ExitFailure n) =
    putStr ("\tOutPut: ") >> printError >>
    putStr ("\tExit Status: ") >> printError
printRes False True ExitSuccess =
    putStr ("\tOutPut: ") >> printError >>
    putStr ("\tExit Status: ") >> printError
printRes False True (ExitFailure n) =
    putStr ("\tOutPut: ") >> printError >>
    putStr ("\tExit Status: ") >> printOk


test :: String -> (ExitCode, String, String) -> IO()
test x (ex, out, err) = do
    print out
    solvedStr <- readFile ("./IntegrationTestFolder/TestFilesSolved/" ++ x ++ ".scm")
    printRes (solvedStr == out) (isInfixOf "error" x) ex


getOutput :: String -> IO()
getOutput x = test x =<< readProcessWithExitCode "cabal" ["run", "glados",
    "echo-args", "--", ("./IntegrationTestFolder/TestFiles/" ++ x ++ ".scm")] ""

loop :: [String] -> IO()
loop [] = putStr ""
loop (x:[]) = putStrLn ("\nTest glados with: " ++ x ++ ".scm") >> getOutput x
loop (x:xs) = putStrLn ("\nTest glados with: " ++ x ++ ".scm") >>  getOutput x >> loop xs


-- integrationSuite :: TestTree
-- integrationSuite = testGroup "Parsing Suite Tests" loop =<< getFiles

-- testfile :: String -> TestTree
-- testfile path = testCaseSteps "test " $ \step -> do
--     step "Preparing..."
--     print path
--     -- let f = "TestFolder/TestFilesSolved/" ++ path
--     -- solvedStr <- readFile f
--     -- print solvedStr
--     gladosStr <- readProcess "cabal run gLaDOS echo-args -- " [path]
--     step "Compare"
--     -- print gladosStr

-- loop :: [String] -> [TestTree]
-- loop (x:[]) = [testfile x]
-- loop (x:xs) = loop xs ++ [testfile x]