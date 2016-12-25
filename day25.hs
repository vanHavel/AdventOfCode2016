import qualified Data.Array.IArray as Array
import Data.Array.IArray (Array)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.RWS

import Utility.InputProcessing

-- data types for interpreting
type Var = String
data Environment = Environment {
    pc :: Int,
    vars :: Map Var Int
} deriving (Show)
makeEnvironment :: Int -> Environment
makeEnvironment i = Environment {pc = 1, vars = Map.fromList [("a", i), ("b", 0), ("c", 0), ("d", 0)]}

data Instruction = 
    Inc Var | 
    Dec Var | 
    Load Int Var | 
    JNZVI Var Int | 
    JNZII Int Int | 
    Assign Var Var |
    OutVar Var |
    OutInt Int
    deriving (Show)
type Code = Array Int Instruction

-- the monad used for interpreting
type Interpret = RWS Code [Int] Environment

-- parse an instruction from a string
parseInstruction :: String -> Instruction
parseInstruction s = let parts = splitOnSpace s in
    case head parts of
        "cpy" -> if elem (parts!!1) varNames
                    then Assign (parts!!1) (parts!!2)
                    else Load (read $ parts!!1) (parts!!2)
        "inc" -> Inc (parts!!1)
        "dec" -> Dec (parts!!1)
        "jnz" -> if elem (parts!!1) varNames
                    then JNZVI (parts!!1) (read $ parts!!2) 
                    else JNZII (read $ parts!!1) (read $ parts!!2)
        "out" -> if elem (parts!!1) varNames
                     then OutVar (parts!!1)
                     else OutInt (read $ parts!!1)
        where
            varNames = ["a", "b", "c", "d"] 

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day25.txt"
    instructionList <- ((map parseInstruction) . lines) <$> readFile file
    let codeArray = Array.listArray (1, length instructionList) instructionList
    let aVal = findSignal codeArray 0
    print aVal
    
-- find value for a that produces clock signal
findSignal :: Code -> Int -> Int
findSignal cmds a | (take 10 . snd $ evalRWS interpret cmds (makeEnvironment a)) 
                    == (take 10 . cycle $ [0, 1]) = a
                  | otherwise = findSignal cmds (succ a)                

-- interpret program and return final environment 
interpret :: Interpret Environment
interpret = do
    env <- get
    code <- ask
    if (pc env) == succ (snd $ Array.bounds code)
        then return env
        else do
            let instruction = code Array.! (pc env)
            execute instruction
            interpret

-- execute a single instruction          
execute :: Instruction -> Interpret ()
execute inst = do
    modify (\env -> env{pc = (pc env) + 1})
    case inst of
        Inc x    -> modify (\env -> env{vars = Map.adjust succ x (vars env)})
        Dec x    -> modify (\env -> env{vars = Map.adjust pred x (vars env)})
        Load i x -> modify (\env -> env{vars = Map.insert x i (vars env)})
        JNZVI x o  -> do
            env <- get
            if ((vars env) Map.! x) /= 0
                then modify (\env -> env{pc = (pc env) - 1 + o})
                else return ()
        JNZII i o -> if i /= 0
            then modify (\env -> env{pc = (pc env) - 1 + o})
            else return ()
        Assign x y -> do
            env <- get
            let val = (vars env) Map.! x
            modify (\env -> env{vars = Map.insert y val (vars env)})
        OutVar x -> do
            env <- get
            let val = (vars env) Map.! x
            tell [val]
        OutInt i -> tell [i]


