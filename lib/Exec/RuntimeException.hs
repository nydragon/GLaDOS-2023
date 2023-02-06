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
  show (InvalidFunctionCall a) = "InvalidFunctionCall: " ++ a ++ " is not a function."
  show UndefinedBehaviour = "UndefinedBehaviour: This behaviour is undefined."
  show NotYetImplemented = "NotYetImplemented"
  show (InvalidArgument a b c) = "InvalidArgument: argument " ++ show a ++ ", expected '" ++ b ++ "' but received '" ++ c ++ "'."
  show (InvalidArgumentCount fn) = "InvalidArgumentCount: Function " ++ fn ++ " received an invalid amount of arguments."
  show NullDivision = "NullDivision: Did you not pay attention in math class?"

data InternalException
  = NonAtomicFunctionArgs String [Ast.Expr]
  deriving (Eq)

instance Exception InternalException

instance Show InternalException where
  show :: InternalException -> String
  show (NonAtomicFunctionArgs a b) = "Function " ++ a ++ ": args are not Atoms\n " ++ show b
