module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1 ✓
type Vname = String
--TODO Task 1.2 ✓
type Val = Int 
--TODO Task 1.3 ✓
type State = Map Vname Val

--TODO Task 1.4 ✓
data Instr =
        LOADI Val | LOAD Vname | ADD | STORE Vname | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6 
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (a, b, c) = (a+1, b, x : c)
iexec (LOAD x) (a, b, c) | let y = Data.Map.findWithDefault (-9999999) x b in y == (-9999999) = (a+1, b, c)
                         | otherwise = (a+1, b, (Data.Map.findWithDefault 0 x b) : c)
                
iexec ADD (a, b, x:y:c) = (a+1, b, x+y:c)
iexec (STORE x) (a, b, v:c) = (a+1, insert x v b, c)
iexec (JMP x) (a, b, c) = (a+x+1, b, c)
iexec (JMPLESS i) (a, b, x:y:c) | x > y = (a+i+1, b, c)
                                | otherwise = (a+1, b, c)
iexec (JMPGE i) (a, b, x:y:c) | x <=  y = (a+i+1, b, c)
                              | otherwise = (a+1, b, c)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec (x:xs) (a, b, c) = exec xs (iexec x (a, b, c))
exec [] (a, b, c) = (a, b, c)