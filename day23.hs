import qualified Data.Array.IArray as Array
import Data.Array.IArray (Array)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import Utility.InputProcessing

-- data types for interpreting
type Var = String

data Instruction = 
    Inc Var | 
    Dec Var | 
    Load Int Var | 
    JNZVI Var Int | 
    JNZII Int Int | 
    Assign Var Var | 
    Toggle Var | 
    InvalidLoad Int Int | 
    InvalidAssign Var Int |
    JNZVV Var Var |
    JNZIV Int Var |
    Add Var Var |
    Mul Var Var |
    Nop
    deriving (Show)
type Code = Array Int Instruction

-- toggling an instruction
toggle :: Instruction -> Instruction
toggle instruction = case instruction of
    Inc x -> Dec x
    Dec x -> Inc x
    Toggle x -> Inc x
    Load i x -> JNZIV i x
    JNZVI x i -> InvalidAssign x i
    JNZII i j -> InvalidLoad i j
    Assign x y -> JNZVV x y
    InvalidLoad i j -> JNZII i j
    InvalidAssign x i -> JNZVI x i
    JNZVV x y -> Assign x y
    JNZIV i x -> Load i x
    _ -> error "Error: toggling a modified instruction."

-- interpreter environment
data Environment = Environment {
    pc :: Int,
    vars :: Map Var Int,
    code :: Code
} deriving (Show)
initialEnvironment1 :: Code -> Environment
initialEnvironment1 cmds = Environment {pc = 1, vars = Map.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)], code = cmds}
initialEnvironment2 :: Code -> Environment
initialEnvironment2 cmds = Environment {pc = 1, vars = Map.fromList [("a", 12), ("b", 0), ("c", 0), ("d", 0)], code = cmds}

-- the monad used for interpreting
type Interpret = State Environment

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
                    then if elem (parts!!2) varNames 
                        then JNZVV (parts!!1) (parts!!2)
                        else JNZVI (parts!!1) (read $ parts!!2)
                    else if elem (parts!!2) varNames
                        then JNZIV (read $ parts!!1) (parts!!2)
                        else JNZII (read $ parts!!1) (read $ parts!!2)
        "tgl" -> Toggle (parts!!1)
        "add" -> Add (parts!!1) (parts!!2)
        "mul" -> Mul (parts!!1) (parts!!2)
        "nop" -> Nop
        where
            varNames = ["a", "b", "c", "d"] 

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day23_mod.txt"
    instructionList <- ((map parseInstruction) . lines) <$> readFile file
    let codeArray = Array.listArray (1, length instructionList) instructionList
        finalEnvironment1 = evalState interpret (initialEnvironment1 codeArray) 
        finalEnvironment2 = evalState interpret (initialEnvironment2 codeArray) 
    print (Map.lookup "a" $ vars finalEnvironment1)
    print (Map.lookup "a" $ vars finalEnvironment2)

-- interpret program and return final environment 
interpret :: Interpret Environment
interpret = do
    env <- get
    if (pc env) == succ (snd $ Array.bounds $ code env)
        then return env
        else do
            let instruction = (code env) Array.! (pc env)
            execute instruction
            interpret

-- execute a single instruction          
execute :: Instruction -> Interpret ()
execute inst = do
    modify (\env -> env {pc = (pc env) + 1})
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
        JNZVV x y -> do
            env <- get
            if ((vars env) Map.! x) /= 0
                then do
                    let o = (vars env) Map.! y
                    modify (\env -> env{pc = (pc env) - 1 + o})
                else return ()
        JNZIV i x -> if i /= 0
            then do
                env <- get
                let o = (vars env) Map.! x
                modify (\env -> env{pc = (pc env) - 1 + o})
            else return ()
        Assign x y -> do
            env <- get
            let val = (vars env) Map.! x
            modify (\env -> env{vars = Map.insert y val (vars env)})
        Toggle x -> do
            env <- get
            let val = (vars env) Map.! x
                target = (pc env) - 1 + val
                targetInstruction = (code env) Array.! target
            if target < (fst . Array.bounds $ code env) || target > (snd . Array.bounds $ code env)
                then return ()
                else do
                    modify (\env -> env {code = (code env) Array.// [(target, toggle targetInstruction)]})
        Add x y -> do
            env <- get
            let valx = (vars env) Map.! x
            modify (\env -> env {vars = Map.adjust (+valx) y (vars env)})
        Mul x y -> do
            env <- get
            let valx = (vars env) Map.! x
            modify (\env -> env {vars = Map.adjust (*valx) y (vars env)})
        _ -> return ()


