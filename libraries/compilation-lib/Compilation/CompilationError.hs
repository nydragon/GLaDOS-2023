{-# LANGUAGE InstanceSigs #-}

module Compilation.CompilationError where

data CompilationError =
    NotAFunction String
    | VariableAlreadyDefined String
    | FatalError
    | Unimplemented

instance Show CompilationError where
    show :: CompilationError -> String
    show (NotAFunction name) = "Error: " ++ name ++ " cannot be compiled to function."
    show (VariableAlreadyDefined name) = "Error: " ++ name ++ " has already been defined."
    show FatalError = "Error: Fatal Error."
    show Unimplemented = "Error: This feature of the language has not yet been implemented."