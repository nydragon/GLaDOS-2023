module Test where

import Data.List
import System.FilePath
import System.Exit
import System.Directory
import System.Process

import Print

getFiles :: IO [String]
getFiles = map takeBaseName . filter (isSuffixOf ".scm") <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents "./tests/integration-tests/TestFiles/"

getOutput :: String -> IO Bool
getOutput fn = do
  callCommand ("./glados_lnk ./tests/integration-tests/TestFiles/" ++ fn ++ ".scm")
  test fn =<< readProcessWithExitCode "./runner_lnk" [ ("a.out") ] ""

test :: String -> (ExitCode, String, String) -> IO Bool
test fn (ex, out, err) = do
  solvedStr <- readFile ("./tests/integration-tests/TestFiles/" ++ fn ++ ".scm.slv")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex
  putStrLn fn
  return (solvedStr == out)