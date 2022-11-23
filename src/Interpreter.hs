module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp = 
    N Val |
    V Vname |
    Plus AExp AExp

    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N x) s = x
aval (V x) s = Data.Map.findWithDefault 0 x s
aval (Plus x y) s = aval x s + aval y s


--TODO Task 2.1
data BExp =
    Bc Bool |
    Not BExp |
    And BExp BExp |
    Less AExp AExp

    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc True) s = True
bval (Bc False) s = False
bval (Not (Bc True)) s = False
bval (Not (Bc False)) s = True
bval (And (Bc True) (Bc True)) s = True
bval (And b1 b2) s | let out1 = bval b1 s
                         out2 = bval b2 s
                     in (out1 == True && out2 == True) = True
                    | otherwise = False
bval (Less a1 a2) s | (aval a1 s) < (aval a2 s) = True
                    | otherwise = False


--TODO Task 2.1
data Com =
    Assign Vname AExp |
    Seq Com Com |
    If BExp Com Com |
    While BExp Com |
    SKIP

    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign v x) s = insert v (aval x s) s
eval (Seq c1 c2) s = eval c2 (eval c1 s)
eval (If b c1 c2) s | let isTrue = bval b s in isTrue == True = eval c1 s
                    | otherwise = eval c2 s
eval (While b c) s | let isTrue = bval b s in isTrue == True = eval (While b c) (eval c s)
                   | otherwise = s
eval SKIP s = s