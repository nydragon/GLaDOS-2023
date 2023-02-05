module Main where

import Data.List
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = do
  putStrLn "Integration Test:\t\t\tOutput\tExit Status\n"
  loop =<< getFiles

getFiles :: IO [String]
getFiles = map takeBaseName . filter (`notElem` [".", ".."]) <$> getDirectoryContents "./IntegrationTestFolder/TestFiles/"

printOk :: IO ()
printOk =
  setSGR [SetColor Foreground Vivid Green]
    >> putStr ("OK")
    >> setSGR [Reset]

printError :: IO ()
printError =
  setSGR [SetColor Foreground Vivid Red]
    >> putStr ("Error")
    >> setSGR [Reset]

printRes :: Bool -> Bool -> ExitCode -> IO ()
printRes True False ExitSuccess = printOk >> putStr "\t" >> printOk >> putStr "\n"
printRes True False (ExitFailure n) = printOk >> putStr "\t" >> printError >> putStr "\n"
printRes True True ExitSuccess = printOk >> putStr "\t" >> printError >> putStr "\n"
printRes True True (ExitFailure n) = printOk >> putStr "\t" >> printOk >> putStr "\n"
printRes False False ExitSuccess = printError >> putStr "\t" >> printOk >> putStr "\n"
printRes False False (ExitFailure n) = printError >> putStr "\t" >> printError >> putStr "\n"
printRes False True ExitSuccess = printError >> putStr "\t" >> printError >> putStr "\n"
printRes False True (ExitFailure n) = printError >> putStr "\t" >> printOk >> putStr "\n"

test :: String -> (ExitCode, String, String) -> IO ()
test fn (ex, out, err) = do
  solvedStr <- readFile ("./IntegrationTestFolder/TestFilesSolved/" ++ fn ++ ".scm")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex

getOutput :: String -> IO ()
getOutput fn =
  test fn
    =<< readProcessWithExitCode
      "cabal"
      [ "run",
        "glados",
        "echo-args",
        "--",
        ("./IntegrationTestFolder/TestFiles/" ++ fn ++ ".scm")
      ]
      ""

loop :: [String] -> IO ()
loop [] = putStr ""
loop (x : xs) = putStr ("Test glados with: " ++ x ++ ".scm\t\t") >> getOutput x >> loop xs
