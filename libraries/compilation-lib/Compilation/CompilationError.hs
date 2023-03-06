{-# LANGUAGE InstanceSigs #-}

module Compilation.CompilationError where

data CompilationError =
    NotAFunction String
    | FatalError

instance Show CompilationError where
    show :: CompilationError -> String
    show FatalError = "Error: Fatal Error"
    show (NotAFunction name) = "Error: " ++ name ++ " cannot be compiled to function."