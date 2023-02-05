module Main where

import Test

main :: IO ()
main = do
  putStrLn "Integration Test:\t\t\tOutput\tExit Status\n"
  loop =<< getFiles

loop :: [String] -> IO ()
loop [] = putStr ""
loop (x : xs) = putStr ("Test glados with: " ++ x ++ ".scm\t\t") >> getOutput x >> loop xs
