import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List

import Utility.InputProcessing

-- maps for storing curent bot values and the passing rules
type OutputMap = Map Int Int
type BotMap    = Map Int [Int]
data Vals = Vals {
    bots   :: BotMap,
    output :: OutputMap
} deriving Show
data Target = Output Int | Bot Int
type PassMap   = Map Int (Target, Target)

-- type for responsibility rules
data BotRule = BotRule Int Int Int
instance Show BotRule where
    show (BotRule bot lo hi) = "Bot " ++ show bot ++ " compares values " ++ show lo ++ " and " ++ show hi ++ "."
-- monad which reads from the passing rules and keeps track of the values
type BotPass = RWS PassMap [BotRule] Vals 

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day10.txt"
    s <- readFile file
    let (initialVals, passMap) = parseInput $ lines s
    let (endVals, responsibilities) = evalRWS processBots passMap initialVals
    let solution1 = find (\(BotRule bot lo hi) -> lo == 17 && hi == 61) responsibilities
    case solution1 of
        Nothing   -> putStrLn ("No bot compares values 17 and 61.")
        Just rule -> do 
            putStrLn $ show rule 
            let solution2 = ((output endVals)!0) * ((output endVals)!1) * ((output endVals)!2)
            putStrLn $ show solution2
    
-- process the bot exchanges until no bot has two values
processBots :: BotPass Vals
processBots = do
    vals <- get
    -- find bot with two values
    let readyBots = Map.filter (\xs -> length xs == 2) (bots vals)
    if null readyBots 
        then return vals
        else do
            mapM_ handleBot $ Map.toList readyBots
            processBots
            
-- handle value passing of a bot
handleBot :: (Int, [Int]) -> BotPass ()
handleBot (bot, [lo, hi]) = do
    rules <- ask
    -- lookup targets in rule
    let Just (loTarget, hiTarget) = Map.lookup bot rules
    -- delete vals of bot 
    modify (\vals -> vals{bots = Map.insert bot [] $ bots vals})
    -- pass vals to targets
    modify (passVal lo loTarget)
    modify (passVal hi hiTarget)
    -- store responsibility
    tell [BotRule bot lo hi]
    
-- passing of a token to a target
passVal :: Int -> Target -> Vals -> Vals
passVal val target (Vals bots output) = 
    case target of 
        Output i -> (Vals bots (Map.insert i val output))
        Bot i    -> (Vals (Map.insertWith (\ a b -> sort (a ++ b)) i [val] bots) output)
    

-- parse the input, building value and rule maps
parseInput :: [String] -> (Vals, PassMap)
parseInput [] = (Vals {bots = Map.empty, output = Map.empty}, Map.empty)
parseInput (s:ss) = let (Vals ssBots ssOut, ssRules) = parseInput ss 
                        splitS = splitOnSpace s in
                            case length splitS of
                                6 ->  (Vals {bots = updateVals splitS ssBots, output = ssOut}, ssRules)
                                12 -> (Vals ssBots ssOut, updateRules splitS ssRules)
                            where updateVals splitS  = Map.insertWith (\a b -> sort (a ++ b)) (read $ splitS!!5) [(read $ splitS!!1)]
                                  updateRules splitS = Map.insert (read $ splitS!!1) (loTarget splitS, hiTarget splitS)
                                  loTarget splitS    = if (splitS!!5)  == "bot" then Bot (read $ splitS!!6)  else Output (read $ splitS!!6)
                                  hiTarget splitS    = if (splitS!!10) == "bot" then Bot (read $ splitS!!11) else Output (read $ splitS!!11)