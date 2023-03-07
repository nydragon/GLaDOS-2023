module Test where

import Data.List
import System.FilePath
import System.Exit
import System.Directory
import System.Process

import Print

getFiles :: IO [String]
getFiles = map takeBaseName . filter (isSuffixOf ".scm") <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents "./integration-tests/TestFiles/"

getOutput :: String -> IO Bool
getOutput fn =
  test fn
    =<< readProcessWithExitCode
      "./runner_lnk"
      [ ("./integration-tests/TestFiles/" ++ fn ++ ".scm") ]
      ""

test :: String -> (ExitCode, String, String) -> IO Bool
test fn (ex, out, err) = do
  solvedStr <- readFile ("./integration-tests/TestFiles/" ++ fn ++ ".scm.slv")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex
  putStrLn fn
  return (solvedStr == out)