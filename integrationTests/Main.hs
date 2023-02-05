module Main where

import Test

main :: IO ()
main = do
  putStrLn "Integration Test:\n\nOutput\tStatus\tName\n"
  loop =<< getFiles

loop :: [String] -> IO ()
loop [] = putStr ""
loop (x : xs) = getOutput x >> loop xs
