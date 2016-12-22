import qualified Data.Array.IArray as Array
import Data.Array.IArray (Array)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Reader

import Utility.InputProcessing

-- data types for interpreting
type Var = String
data Environment = Environment {
    pc :: Int,
    vars :: Map Var Int
} deriving (Show)
initialEnvironment1 :: Environment
initialEnvironment1 = Environment {pc = 1, vars = Map.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]}
initialEnvironment2 :: Environment
initialEnvironment2 = Environment {pc = 1, vars = Map.fromList [("a", 0), ("b", 0), ("c", 1), ("d", 0)]}

data Instruction = Inc Var | Dec Var | Load Int Var | JNZ Var Int | JNZI Int Int | Assign Var Var 
    deriving (Show)
type Code = Array Int Instruction

-- the monad used for interpreting
type Interpret = ReaderT Code (State Environment)

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
                    then JNZ (parts!!1) (read $ parts!!2) 
                    else JNZI (read $ parts!!1) (read $ parts!!2)
        where
            varNames = ["a", "b", "c", "d"] 

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day12.txt"
    instructionList <- ((map parseInstruction) . lines) <$> readFile file
    let codeArray = Array.listArray (1, length instructionList) instructionList
        finalEnvironment1 = evalState (runReaderT interpret codeArray) initialEnvironment1
        finalEnvironment2 = evalState (runReaderT interpret codeArray) initialEnvironment2      
    print (Map.lookup "a" $ vars finalEnvironment1)
    print (Map.lookup "a" $ vars finalEnvironment2)

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
        JNZ x o  -> do
            env <- get
            if ((vars env) Map.! x) /= 0
                then modify (\env -> env{pc = (pc env) - 1 + o})
                else return ()
        JNZI i o -> if i /= 0
            then modify (\env -> env{pc = (pc env) - 1 + o})
            else return ()
        Assign x y -> do
            env <- get
            let val = (vars env) Map.! x
            modify (\env -> env{vars = Map.insert y val (vars env)})


