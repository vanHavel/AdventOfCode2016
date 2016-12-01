import Control.Monad.State
import Control.Monad.Writer

import Utility.InputProcessing
import Utility.Lists

-- data type for commands given in the input list
data Command = TurnRight Int | TurnLeft Int

-- data type for directions
data Direction = North | East | South | West
-- direction updates
turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North
turnLeft :: Direction -> Direction
turnLeft  North = West
turnLeft  West  = South
turnLeft  South = East
turnLeft  East  = North

-- data type for my location, with coordinates (x,y) and the direction I am facing
data Location = Location {
    x :: Int,
    y :: Int,
    direction :: Direction
}
-- my starting location
initialLocation :: Location
initialLocation = Location {
    x = 0,
    y = 0,
    direction = North
}  

-- the monad for the trip through the city: a Writer that writes all visited positions on top of a State that stores the current location
type Walker = WriterT [(Int, Int)] (State Location)

-- solve problems of day 1 and 2
main :: IO()
main = do
    let file = "input/input_day1.txt"
    s <- readFile file
    let commands = map toCommand $ splitOnComma $ dropSpaces s
    let (dist1, visited) = evalState (runWriterT (distance commands)) initialLocation
    putStrLn $ "Problem one solution: " ++ show dist1
    case firstRepetition visited of
        Nothing    -> putStrLn "Error: no location was visited twice!"
        Just (x,y) -> do 
            let dist2 = (abs x) + (abs y)
            putStrLn $ "Problem two solution: " ++ show dist2
    
-- calculate distance given list of commands
distance :: [Command] -> Walker Int
distance commands = do
    tell [(0,0)]
    mapM_ runCommand commands
    location <- get
    return $ (abs $ x location) + (abs $ y location)
    
-- execute a single command
runCommand :: Command -> Walker ()
runCommand (TurnRight i) = do
    location <- get
    modify (\location -> location{direction = turnRight $ direction location})
    walk i
runCommand (TurnLeft  i) = do
    location <- get
    modify (\location -> location{direction = turnLeft $ direction location})
    walk i

-- walk a given distance forward in the grid   
walk :: Int -> Walker ()
walk 0 = return ()
walk i = do
    location <- get
    case direction location of
        North -> modify (\location -> location{y = (y location) + 1})
        East  -> modify (\location -> location{x = (x location) + 1})
        South -> modify (\location -> location{y = (y location) - 1})
        West  -> modify (\location -> location{x = (x location) - 1})
    location <- get
    tell [(x location, y location)]
    walk (i - 1)
    
-- turn a string representation into a command
toCommand :: String -> Command
toCommand ('L':len) = TurnLeft  (read len)
toCommand ('R':len) = TurnRight (read len)
                        