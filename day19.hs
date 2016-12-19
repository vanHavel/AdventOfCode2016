import Data.List

import Utility.Lists

-- initial situation
initial :: [Int]
initial = [1..3017957]

-- perform one stealing step
step :: [Int] -> [Int]
step list = if even . length $ list then dropEverySecond list else tail $ dropEverySecond list

-- perform one stealing step for puzzle 2
step2 :: [Int] -> [Int]
step2 [x] = [x]
step2 list@(x:xs) = let n = length list in if mod n 6 == 4 then threeStep n list else step2 $ delete (list!!(div n 2)) (tail list ++ [x]) where
    threeStep n xs = let (first, second) = splitAt (div n 2) xs in
                         reShift n . dropTwoThirds $ second ++ first
    dropTwoThirds [x] = [x]
    dropTwoThirds (x:y:z:xs) = z:(dropTwoThirds xs)
    reShift n xs = rotateLeft (div ((div n 2) - 2) 3) xs
        
    
-- perform steps until one elf is left
steps :: [Int] -> Int
steps [lastId] = lastId
steps xs = steps . step $ xs
steps2 :: [Int] -> Int
steps2 [lastId] = lastId
steps2 xs = steps2 . step2 $ xs

-- find solutions to the puzzles
main :: IO ()
main = do
    print $ steps initial
    print $ steps2 initial