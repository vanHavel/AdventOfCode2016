import Control.Monad.State

import Data.Char

import Utility.InputProcessing

-- data type for different keypad layouts
data Layout = Layout1 | Layout2

-- data type for direction to move
data Direction = GoRight | GoDown | GoLeft | GoUp

-- turn char into direction
toDirection :: Char -> Direction
toDirection 'R' = GoRight
toDirection 'D' = GoDown
toDirection 'L' = GoLeft
toDirection 'U' = GoUp
 
-- data type for position on keypad, x and y coordinated, each value between 0 and 2
type Position = (Int, Int)
-- initial positions (number 5) on layout2 1 and 2
initialPosition1 :: Position
initialPosition1 = (1, 1)
initialPosition2 :: Position
initialPosition2 = (0, 2)

-- move position given a direction and layout
move :: Layout -> Position -> Direction -> Position
move Layout1 pos dir = move1 pos dir
move Layout2 pos dir = move2 pos dir

-- move position given a direction, on layout 1
move1 :: Position -> Direction -> Position
move1 (x, y) GoRight | x == 2    = (x, y)
                     | otherwise = (x + 1, y)
move1 (x, y) GoDown  | y == 2    = (x, y)
                     | otherwise = (x, y + 1)
move1 (x, y) GoLeft  | x == 0    = (x, y)
                     | otherwise = (x - 1, y)
move1 (x, y) GoUp    | y == 0    = (x, y)
                     | otherwise = (x, y - 1)
                     
-- move position given a direction, on layout 2
move2 :: Position -> Direction -> Position
move2 pos@(x, y) GoRight | elem pos rightBorder = (x, y)
                         | otherwise            = (x + 1, y)
                           where rightBorder    = [(2, 0), (3, 1), (4, 2), (3, 3), (2, 4)]
move2 pos@(x, y) GoDown  | elem pos downBorder  = (x, y)
                         | otherwise            = (x, y + 1)       
                           where downBorder     = [(0, 2), (1, 3), (2, 4), (3, 3), (4, 2)]                                       
move2 pos@(x, y) GoLeft  | elem pos leftBorder  = (x, y)
                         | otherwise            = (x - 1, y)  
                           where leftBorder     = [(2, 0), (1, 1), (0, 2), (1, 3), (2, 4)]
move2 pos@(x, y) GoUp    | elem pos upBorder    = (x, y)
                         | otherwise            = (x, y - 1)                     
                           where upBorder       = [(0, 2), (1, 1), (2, 0), (3, 1), (4, 2)]
                           
-- get keypad number from position, given layout 
number :: Layout -> Position -> Char
number Layout1 (x, y) = intToDigit $ y * 3 + x + 1
number Layout2 (x, y) = case y of
    0 -> '1'
    1 -> intToDigit $ 1 + x
    2 -> intToDigit $ 5 + x
    3 -> chr $ (ord 'A' + (x - 1))
    4 -> 'D'

-- find solutions for puzzles 1 and 2
main :: IO ()
main = do
    let file = "input/input_day2.txt"
    s <- readFile file
    let sequences = map (map toDirection) (lines s)
    let code1 = evalState (findCode Layout1 sequences) initialPosition1
    putStrLn $ show code1
    let code2 = evalState (findCode Layout2 sequences) initialPosition2
    putStrLn $ show code2

-- get the keypad code from the directions
findCode :: Layout -> [[Direction]] -> State Position [Char]
findCode layout dss = mapM (getDigit layout) dss

-- get one digit of the code
getDigit :: Layout -> [Direction] -> State Position Char
getDigit layout ds = do
    mapM_ (step layout) ds 
    position <- get
    return $ number layout position
    
-- make one move on the keypad
step :: Layout -> Direction -> State Position ()
step layout direction = modify (\position -> move layout position direction)
    
    