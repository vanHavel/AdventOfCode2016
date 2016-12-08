import Control.Monad.State
import Utility.InputProcessing
import Data.List
import Utility.Lists

-- grid data type
newtype Grid = Grid [[Bool]]
instance Show Grid where
    show (Grid []) = []
    show (Grid (r:rs)) = (map (\b -> if b then '#' else '.') r) ++ "\n" ++ show (Grid rs)
initialGrid :: Grid
initialGrid = Grid [[False | cols <- [1..50]] | rows <- [1..6]]

-- command data type
data Command = Rect Int Int | RotateRow Int Int | RotateCol Int Int

-- parse a command from string representation
parseCommand :: String -> Command
parseCommand s = let splits = splitOnSpace s in
    case length splits of
        2 -> let rect = splitOnCross (splits!!1) in
            Rect (read (rect!!0)) (read (rect!!1))
        5 -> let rot = ((splitOnEquals (splits!!2))!!1) in
            case (splits!!1) of
                "row"    -> RotateRow (read rot) (read (splits!!4))
                "column" -> RotateCol (read rot) (read (splits!!4))

-- execute a command on a grid
executeCommand :: Grid -> Command -> Grid
executeCommand (Grid grid) (Rect x y) = Grid $ (map (set x) upToY) ++ fromY where 
    (upToY, fromY) = splitAt y grid
    set :: Int -> [Bool] -> [Bool] 
    set 0 bs = bs
    set n (b:bs) = True:(set (n-1) bs)
executeCommand (Grid grid) (RotateRow y l) = Grid $ beforeY ++ ((rotate l rowY):fromY) where
    (beforeY, rest)  = splitAt y grid
    ([rowY], fromY) = splitAt 1 rest
executeCommand (Grid grid) (RotateCol x l) = Grid $ transpose grid' where
    Grid grid' = executeCommand (Grid $ transpose grid) (RotateRow x l)

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day8.txt"
    s <- readFile file
    let commands = map parseCommand $ lines s
    let finalGrid = evalState (runCommands commands) initialGrid
    let lit = length $ filter (== True) $ concat fGrid where
        (Grid fGrid) = finalGrid
    putStrLn $ show lit
    putStrLn $ show finalGrid
    
-- run a list of commands on a grid, return resulting grid
runCommands :: [Command] -> State Grid Grid
runCommands cmds = do
    mapM_ runCommand cmds
    get
    
-- run a single command
runCommand :: Command -> State Grid ()
runCommand cmd = do 
    modify (\grid -> executeCommand grid cmd) 