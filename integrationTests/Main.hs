module Main where

import Test

import System.Exit ( exitSuccess, exitFailure )
main :: IO ()
main = do
  putStrLn "Integration Test:\n\nOutput\tStatus\tName\n"
  result <- loop =<< getFiles

  if result
    then do exitSuccess
    else exitFailure

getValidity :: Bool -> Bool -> Bool
getValidity False _ = False
getValidity _ False = False
getValidity _ _ = True

loop' :: [String] -> Bool -> IO Bool
loop' [] valid = return valid
loop' (x : xs) valid = do
    res <- getOutput x
    loop' xs $ getValidity res valid

loop :: [String] -> IO Bool
loop str = loop' str True