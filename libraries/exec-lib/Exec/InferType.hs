{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Exec.InferType where

import qualified Data.Map as Map
data Type 
    = String String
    | Float Float
    | Integer Integer
    | Symbol String
    | List [Type]
    | Boolean Bool
    | Null
    deriving (Ord, Eq)
instance Num Type where
  (+) :: Type -> Type -> Type
  (+) (Integer a) (Integer b) = Integer (a + b)
  (+) (Integer a) (Float b) = Float (fromInteger a + b)
  (+) (Float a) (Integer b) = Float (a + fromInteger b)
  (+) (Float a) (Float b) = Float (a + b)

  (-) :: Type -> Type -> Type
  (-) (Integer a) (Integer b) = Integer (a - b)
  (-) (Integer a) (Float b) = Float (fromInteger a - b)
  (-) (Float a) (Integer b) = Float (a - fromInteger b)
  (-) (Float a) (Float b) = Float (a - b)

  (*) :: Type -> Type -> Type
  (*) (Integer a) (Integer b) = Integer (a * b)
  (*) (Integer a) (Float b) = Float (fromInteger a * b)
  (*) (Float a) (Integer b) = Float (a * fromInteger b)
  (*) (Float a) (Float b) = Float (a * b)

instance Fractional Type where
  (/) :: Type -> Type -> Type
  (/) (Integer a) (Integer b) = Float $ fromIntegral (a `div` b)
  (/) (Integer a) (Float b) = Float (fromInteger a / b)
  (/) (Float a) (Integer b) = Float (a / fromInteger b)
  (/) (Float a) (Float b) = Float (a / b)

instance Show Type where
  show :: Type -> String
  show (String a) = init $ tail a
  show (Float a) = show a
  show (Integer a) = show a
  show (Symbol a) = a
  show (List a) = show a
  show (Boolean a) | a = "#t" | otherwise = "#f"
  show Null = "Null"

type Registers = Map.Map Type Type

type ArgStack = [Type]

data StackFrame = StackFrame { localVars :: Registers, parentFunc :: String, parentOffset :: Integer}
  deriving (Show, Eq)

makeStackFrame :: String -> Integer -> StackFrame
makeStackFrame a b = StackFrame { localVars = Map.empty, parentFunc = a, parentOffset = b}

data Stack = Stack [StackFrame] ArgStack Registers
  deriving (Show, Eq)

initStack :: Stack
initStack = Stack [] [] Map.empty
