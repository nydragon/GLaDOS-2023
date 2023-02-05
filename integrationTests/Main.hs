module Main where

import Data.List
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = do
  putStrLn "Integration Test:\n\nOutput\tStatus\tName\n"
  loop =<< getFiles

getFiles :: IO [String]
getFiles = map takeBaseName . filter (isSuffixOf ".scm") <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents "./IntegrationTests/"

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
printRes True False ExitSuccess = printOk >> putStr "\t" >> printOk >> putStr "\t"
printRes True False (ExitFailure n) = printOk >> putStr "\t" >> printError >> putStr "\t"
printRes True True ExitSuccess = printOk >> putStr "\t" >> printError >> putStr "\t"
printRes True True (ExitFailure n) = printOk >> putStr "\t" >> printOk >> putStr "\t"
printRes False False ExitSuccess = printError >> putStr "\t" >> printOk >> putStr "\t"
printRes False False (ExitFailure n) = printError >> putStr "\t" >> printError >> putStr "\t"
printRes False True ExitSuccess = printError >> putStr "\t" >> printError >> putStr "\t"
printRes False True (ExitFailure n) = printError >> putStr "\t" >> printOk >> putStr "\t"

test :: String -> (ExitCode, String, String) -> IO ()
test fn (ex, out, err) = do
  solvedStr <- readFile ("./IntegrationTests/" ++ fn ++ ".scm.slv")
  printRes (solvedStr == out) (isInfixOf "error" fn) ex
  putStrLn fn
  return ()

getOutput :: String -> IO ()
getOutput fn =
  test fn
    =<< readProcessWithExitCode
      "./glados"
      [ ("./IntegrationTests/" ++ fn ++ ".scm")
      ]
      ""

loop :: [String] -> IO ()
loop [] = putStr ""
loop (x : xs) = getOutput x >> loop xs
