module Test where

import Data.List
import System.FilePath
import System.Exit
import System.Directory
import System.Process

import Print

getFiles :: IO [String]
getFiles = map takeBaseName . filter (`notElem` [".", ".."]) <$> getDirectoryContents "./integrationTests/integrationTestFolder/TestFiles/"

getOutput :: String -> IO ()
getOutput fn =
  test fn
    =<< readProcessWithExitCode
      "cabal"
      [ "run",
        "glados",
        "echo-args",
        "--",
        ("./integrationTests/integrationTestFolder/TestFiles/" ++ fn ++ ".scm")
      ]
      ""

test :: String -> (ExitCode, String, String) -> IO ()
test fn (ex, out, err) = do
  solvedStr <- readFile ("./integrationTests/integrationTestFolder/TestFilesSolved/" ++ fn ++ ".scm")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex