module Test where

import Data.List
import System.FilePath
import System.Exit
import System.Directory
import System.Process

import Print

getFiles :: IO [String]
getFiles = map takeBaseName . filter (isSuffixOf ".scm") <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents "./integrationTests/TestFiles/"

getOutput :: String -> IO Bool
getOutput fn =
  test fn
    =<< readProcessWithExitCode
      "./glados"
      [ ("./integrationTests/TestFiles/" ++ fn ++ ".scm") ]
      ""

test :: String -> (ExitCode, String, String) -> IO Bool
test fn (ex, out, err) = do
  solvedStr <- readFile ("./integrationTests/TestFiles/" ++ fn ++ ".scm.slv")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex
  putStrLn fn
  return (solvedStr == out)