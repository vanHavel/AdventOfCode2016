import Data.List

import Utility.InputProcessing
import Utility.Lists

-- find solution to problem 1 and 2
main :: IO ()
main = do
    let file = "input/input_day3.txt"
    s <- readFile file
    let splitLines = map splitOnSpace (lines s)
    let triangles  = map (map read) splitLines
    putStrLn $ show $ countValid triangles
    let transposed = concat $ map (splitEvery 3) (transpose splitLines)
    let triangles2 = map (map read) transposed
    putStrLn $ show $ countValid triangles2

-- count number of valid triangles    
countValid :: [[Int]] -> Int
countValid = length . filter isValidTriangle

-- checks whether three sides can make a triangle
isValidTriangle :: [Int] -> Bool
isValidTriangle [a, b, c] = (a + b > c) && (a + c > b) && (b + c > a)