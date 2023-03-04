module Exec where

import Control.Exception (throwIO)
import Debug.Trace

import Exec.Function (lookupFunc)
import Exec.Registry (Registry, RetVal (..))
import Exec.RuntimeException
import qualified Parsing.Ast as Ast

-- ─── Function Execution ──────────────────────────────────────────────────────────────────────────

regFromRet :: RetVal -> Registry
regFromRet (RetVal a b) = a

-- Bind all arguments to their values in preparation of a function call
bindArgs :: [String] -> [Ast.Expr] -> Registry -> IO Registry
bindArgs [] _ reg = return reg
bindArgs _ [] reg = return reg
bindArgs (x:xs) (y:ys) (v, f) = do
    RetVal reg ret <- defineVar [Ast.Symbole x, y] reg

    bindArgs xs ys reg
    where
        reg = (v, f)

-- Unbinds list of variable names it is given on entry
unbindArgs :: [String] -> RetVal -> IO RetVal
unbindArgs vars (RetVal reg ret) = return newRet
    where
        newRet = RetVal (removeVars vars reg) ret

