module Main where

data Cpt = Integer Int
    | Symbol String
    | List [Cpt]

main :: IO ()
main = putStrLn "Hello, Haskell!"
