import Data.List
import Utility.Lists

-- solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day6.txt"
    s <- readFile file
    let cols = transpose $ lines s
    let solution1 = map mode cols
    putStrLn solution1
    let solution2 = map leastCommon cols
    putStrLn solution2