module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]  
acomp (V v) = [LOAD v]
acomp (Plus a b) = (acomp a) ++ (acomp b) ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b) t i | b == t = [JMP i]
                 | otherwise = []
bcomp (Not b) t i | let out = bcomp b t i in out == [] = [JMP i]
                  | otherwise = []

-- | let isTrue = bval b s in isTrue == True = bcomp (Bc False) t i
-- | b == False = bcomp (Bc True) t i

-- bcomp (And (Bc b1) (Bc b2)) t i | (b1 && b2 == t) = [JMP i]
-- | otherwise = []

bcomp (And (Less a1 a2) (Less a3 a4)) True i = [JMP i] ++ bcomp (Less a1 a2) True i ++ bcomp (Less a3 a4) True i
bcomp (And (Less a1 a2) _) True i = [JMP i] ++ bcomp (Less a1 a2) True i 
bcomp (And _ (Less a1 a2)) True i = [JMP i] ++ bcomp (Less a1 a2) True i
bcomp (And (Less a1 a2) (Less a3 a4)) False i = bcomp (Less a1 a2) False i ++ bcomp (Less a3 a4) False i
bcomp (And (Less a1 a2) _) False i = bcomp (Less a1 a2) False i
bcomp (And (Bc False) (Less a1 a2)) False i = let j = i*2 in [JMP j] ++bcomp (Less a1 a2) False i
bcomp (And b1 b2) True i | let out1 = (bcomp b1 True i) 
                               out2 = (bcomp b2 True i) 
                           in (out1 == [JMP i] && out2 == [JMP i]) = [JMP i]
                         | let out1 = (bcomp b1 True i) 
                               out2 = (bcomp b2 True i) 
                           in (out1 == [] && out2 == [JMP i]) = [JMP 1, JMP i]
                         | otherwise = []
bcomp (And b1 b2) False i | let out1 = (bcomp b1 False i) 
                                out2 = (bcomp b2 False i) 
                            in (out1 == [] && out2 == []) = []
                          | otherwise = [JMP i]
 


bcomp (Less a1 a2) t i | t == True = acomp(a1) ++ acomp(a2) ++ [JMPLESS i]
                       | t == False = acomp(a1) ++ acomp(a2) ++ [JMPGE i]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign a b) = acomp b ++ [STORE a]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2   
ccomp (If (Less a1 a2) c1 c2) | (bcomp (Less a1 a2) False 5) == [] = (bcomp (Less a1 a2) False 5) ++ ccomp c1
                              | otherwise = (bcomp (Less a1 a2) False 5) ++ ccomp c1 ++ [JMP 2] ++ ccomp c2
ccomp (If b c1 c2) | (bcomp b False 1) == [] = (bcomp b False 1) ++ ccomp c1
                              | otherwise = (bcomp b False 1) ++ ccomp c1 ++ [JMP 2] ++ ccomp c2
ccomp (While (Less a1 a2) c) = let len = (bcomp (Less a1 a2) False 5) ++ ccomp c in (bcomp (Less a1 a2) False 5) ++ ccomp c ++ [JMP (-(length (len) + 1))]
ccomp (SKIP) = []