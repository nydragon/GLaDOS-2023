module Compilation.Compile where

import qualified Parsing.Ast as Ast
import Compilation.Utils ( isAtomic )
import Compilation.RetVal ( RetVal(..), concatRetVal )
import Compilation.Registry
import Compilation.CompilationError
import FunctionBlock ( FunctionBlock(..) )
import Instruction ( Instruction(Move, Call, Push, Init) )
import Control.Exception ( throw )

-- ─── Compile Expression ──────────────────────────────────────────────────────────────────────────

-- Compiles and Ast Expression
--
-- This is essentially the main function for compilation,
--    asides from the compileProgram func
-- Args : Expression -> Registry -> Defined Functions
-- Return : (Instructions (if not function def), Updated function definitions)
compileExpr :: Ast.Expr -> Registry -> RetVal
-- Lambda called on definition
-- compileExpr (Ast.ExprList (Ast.Call "lambda" [Ast.ExprList args, body] : argValues)) reg funcs = evalLambda args body argValues reg
-- Basic ExprList
-- compileExpr (Ast.ExprList ls) reg funcs = evalExprList ls reg
-- Function definition
-- compileExpr (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg funcs = execCall (Ast.Call "define" (sym : Ast.Call "lambda" args : r)) reg
-- Function Call
compileExpr (Ast.Call funcName args) reg = compileCall call reg
    where call = Ast.Call funcName args
-- Variable definition
compileExpr (Ast.Call "define" ((Ast.Symbole s) : val)) reg = compileVariable call reg
    where call = Ast.Call "define" (Ast.Symbole s : val)
compileExpr _ _ = throw FatalError

-- ─── Compilation Main Functions ──────────────────────────────────────────────────────────────────

-- Compiles a list of expressions
--
-- This will compile all instructions inside a retval as well as saving additional function declarations
compileExprList' :: Ast.Expr -> Registry -> RetVal
compileExprList' (Ast.ExprList (x:xs)) reg = concatRetVal compiledExpr compiledLeftover
    where
        compiledExpr = compileExpr x reg
        (RetVal _ _ updatedReg) = compiledExpr
        compiledLeftover = compileExprList' (Ast.ExprList xs) updatedReg
compileExprList' (Ast.ExprList []) reg = RetVal [] [] reg
compileExprList' _ _ = throw FatalError

-- Entry point function for compileExprList'
compileExprList :: Ast.Expr -> RetVal
compileExprList (Ast.ExprList ls) = compileExprList' list emptyRegistry
    where list = Ast.ExprList ls
compileExprList _ = throw FatalError

-- Compiles program into list of FunctionBlock INCLUDING main func
-- Arg : ExprList to be compiled (corresponds to base depth level of input code)
compileProgram :: Ast.Expr -> [FunctionBlock]
compileProgram list = output
    where
        (RetVal instrs funcs _) = compileExprList list
        mainFunc = Function "main" instrs
        output = mainFunc : funcs

-- ─── Function Compilation ────────────────────────────────────────────────────────────────────────

-- Compiles an ExprList into instructions required for the function call
-- Args are compiled from last to first
--
-- Args : Ast.ExprList -> Reg
compileCallArgs :: Ast.Expr -> Registry -> RetVal
compileCallArgs (Ast.ExprList []) reg = RetVal [] [] reg
compileCallArgs (Ast.ExprList (x : xs)) reg | isAtomic x = RetVal (instr ++ [Push $ show x]) [] reg
    | otherwise = concatRetVal ret  (RetVal instr [] reg)
    where
        (RetVal instr _ _) = compileCallArgs (Ast.ExprList xs) reg
        ret = compileCall x reg 
compileCallArgs _ _ = throw FatalError

flipL :: [a] -> [a]
flipL = foldl (flip (:)) []

--
-- Compiles function call as well as args (calls compileCallArgs)
--
compileCall :: Ast.Expr -> Registry -> RetVal
compileCall (Ast.Call funcName args) reg | isAtomic (Ast.ExprList args) = RetVal instruction [] reg
    where instruction = [Push $ show arg | arg <- flipL args] ++ [Call funcName, Push "#RET"] 
compileCall (Ast.Call funcName args) reg = RetVal (instructions ++ [Call funcName, Push "#RET"]) [] reg
    where (RetVal instructions _ _) = compileCallArgs (Ast.ExprList args) reg
compileCall _ _ = throw FatalError

-- This function attempts to convert an Ast.ExprList to a Ast.Call
-- If the function does not exist, it returns Nothing
-- Args : Input ExprList + Registry
-- Output : Ast.Call
convertToCall :: Ast.Expr -> Registry -> Maybe Ast.Expr
convertToCall (Ast.ExprList (Ast.Symbole name : ls)) reg = if isFunction name reg
    then Just $ Ast.Call name ls
    else Nothing
convertToCall _ _ = Nothing

-- ─── Variable Compilation ────────────────────────────────────────────────────────────────────────

-- Compiles instructions required for variable compilation
--
-- Note : This does not check the registry if the variable is already defined
-- Args : Ast.Call (Expected to haveproper define form for a variable definition) -> reg -> functions
compileVariable :: Ast.Expr -> Registry -> RetVal
compileVariable (Ast.Call "define" [Ast.Symbole s, Ast.Call name args]) reg = throw Unimplemented
compileVariable (Ast.Call "define" [Ast.Symbole s1, Ast.Symbole s2]) reg = if isVariable s2 reg
    then RetVal instructions [] newReg
    else throw $ VariableNotDefined s2
        where
            newReg = addVar s1 reg
            instructions = [Init s1, Move s1 s2]
compileVariable (Ast.Call "define" [Ast.Symbole s, atom]) reg = if isAtomic atom
    then RetVal instructions [] newReg
    else throw NonAtomicValue
        where
            newReg = addVar s reg
            instructions = [Init s, Move s $ show atom]
compileVariable _ _ = throw FatalError