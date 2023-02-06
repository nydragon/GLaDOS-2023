module Print where

import System.Console.ANSI
import System.Exit

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