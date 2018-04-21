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
module ExprType where

import Data.List

-- * section expr
-- | data type
data Expr a = Add (Expr a) (Expr a)    -- ^ Add - binary addition
            | Mult (Expr a) (Expr a)   -- ^ Mult - binary multiplication
            | Cos (Expr a)             -- ^ Cos - binary multiplication
            | Sin (Expr a)             -- ^ Sin - binary multiplication
            | NatExp (Expr a)          -- ^ NatExp - natural exponent
            | Log (Expr a)             -- ^ Log - log of base a
            | Const a                  -- ^ Const - wraps a constant value
            | Var String               -- ^ Var - wraps a variable identifier
  deriving Eq

 {- `getVars`
   - Given an expression, retrieves a list of all variable identifiers
 -}

getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident) = [ident]
getVars (Cos x) = getVars x
getVars (Sin x) = getVars x
getVars (Log x) = getVars x
getVars (NatExp x) = getVars x