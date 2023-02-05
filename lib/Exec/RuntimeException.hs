{-# LANGUAGE InstanceSigs #-}

module Exec.RuntimeException where

import Control.Exception
import qualified Parsing.Ast as Ast

-- Runtime exception data type
--
-- NOTE: This definition will be completed as development continues
data RuntimeException
  = InvalidFunctionCall String
  | InvalidFunctionDefinition String
  | UndefinedBehaviour -- Pretty much for anything that shouldn't ever happen
  | NotYetImplemented
  | -- args: argument index, expected type, got type
    InvalidArgument Integer String String
  | InvalidArgumentCount String
  | NullDivision
  | FatalError -- For stuff that shouldn't happen
  | AlreadyDefined
  deriving (Eq)

instance Exception RuntimeException

instance Show RuntimeException where
  show :: RuntimeException -> String
  show (InvalidFunctionCall a) = a ++ " is an invalid function call."
  show UndefinedBehaviour = "This is undefined."
  show NotYetImplemented = "OH MAXI BEBOU"
  show (InvalidArgument a b c) = "Argument " ++ show a ++ " invalid: expected '" ++ b ++ "' but received '" ++ c ++ "'."
  show (InvalidArgumentCount fn) = "Function " ++ fn ++ ": Invalid arg count."
  show NullDivision = "Did you not pay attention in math class?"

data InternalException
  = NonAtomicFunctionArgs String [Ast.Expr]
  deriving (Eq)

instance Exception InternalException

instance Show InternalException where
  show :: InternalException -> String
  show (NonAtomicFunctionArgs a b) = "Function " ++ a ++ ": args are not Atoms\n " ++ show b
