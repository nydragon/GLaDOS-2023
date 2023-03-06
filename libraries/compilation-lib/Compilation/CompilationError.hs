{-# LANGUAGE InstanceSigs #-}

module Compilation.CompilationError where

data CompilationError =
    FatalError

instance Show CompilationError where
    show :: CompilationError -> String
    show FatalError = "Fatal Error"