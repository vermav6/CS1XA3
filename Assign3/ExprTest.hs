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

module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

listToExpr1 :: [Double] -> Expr Double
listToExpr1 [x]    = Const x
listToExpr1 (x:xs) = Add (Const x) (listToExpr1 xs)
listToExpr1 []     = error "Not list to expression for empty"

---------------------------
sampleExpr1 :: Expr Double
sampleExpr1 = (var "x") !+ (var "y")


test1 :: Double -> Bool
test1 x = eval (Map.fromList [("x",x),("y",-x)]) sampleExpr1 == 0
--------------------------Squaring a number x-----------

sampleExpr2 :: Expr Double
sampleExpr2 = (var "x") !* (var "y")


test2 :: Double -> Bool
test2 x = eval (Map.fromList [("x",x),("y",x)]) sampleExpr2 == x^2
--------------------------------------------


sampleExpr3 :: Expr Double
sampleExpr3 = myCos (var "a")


test3 :: Double -> Bool
test3 a = eval (Map.fromList [("a",a)]) sampleExpr3 == cos a

------------------------------------------------

sampleExpr4 :: Expr Double
sampleExpr4 = mySin (var "b")


test4 :: Double -> Bool
test4 b = eval (Map.fromList [("b",b)]) sampleExpr4 == sin b
------------------------------------------------