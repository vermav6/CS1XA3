{-|
Module      : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright   : (c) Varun Verma @2018
License     : WTFPL
Maintainer  : vermav6@mcmaster.ca
Stability   : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}

module ExprPretty where

import ExprType

-- | To Wrap the expression in Parenthesis
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

-- | Formats each operation by printing the operator associated with that
instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x) = parens $ "val " ++ show x
  show (Cos ss) = parens $ "cos " ++ show ss
  show (Sin ss) = parens $ "sin " ++ show ss
  show (Log ss) = parens $ "log " ++ show ss
  show (NatExp ss) = parens $ "e^" ++ show ss