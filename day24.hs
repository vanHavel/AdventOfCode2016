import Data.List
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

import Utility.AStar

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day24.txt"
    s <- readFile file
    let field = map parseField $ lines s
        positions = getPositions s
        distances = getDistances positions field
        solution1 = solveTSP distances False
        solution2 = solveTSP distances True
    print solution1
    print solution2

-- parse a field line
parseField :: String -> [Bool]
parseField = map (/= '#')

-- extract position ordered by their index from the field
getPositions :: String -> [(Int, Int)]
getPositions s = map snd . sortBy (compare `on` fst) $ gp (0, 0) (lines s) where
    gp :: (Int, Int) -> [String] -> [(Int, (Int, Int))]
    gp _ [] = []
    gp (y, x) ([]:xs) = gp (y + 1, 0) xs
    gp (y, x) ((z:zs):xs) | elem z ['0'..'9'] = (read [z], (y, x)):(gp (y, x + 1) (zs:xs))
                          | otherwise = gp (y, x + 1) (zs:xs)

-- calculate distance matrix for the positions, given the field
getDistances :: [(Int, Int)] -> [[Bool]] -> [[Int]]
getDistances positions field = map (\pos -> map (distance field pos) positions) positions where
    distance :: [[Bool]] -> (Int, Int) -> (Int, Int) -> Int
    distance field pos1 pos2 = aStarSearch pos1 pos2 (manhattan pos2) (successors field)
    manhattan :: (Int, Int) -> (Int, Int) -> Int
    manhattan (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)
    successors :: [[Bool]] -> (Int, Int) -> Set (Int, Int)
    successors field (y, x) = Set.fromList [(a, b) | (a, b) <- [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)],
                                                     a >= 0 && b >= 0 && a < (length field) && b < (length $ field!!0),
                                                     ((field!!a)!!b)]

-- solve the TSP given by the input matrix, finding minimal HP starting at first position. 
-- If roundTrip is true, then the first position has to be reached again.
solveTSP :: [[Int]] -> Bool -> Int
solveTSP distances roundTrip = tsp distances [0] roundTrip where
    tsp :: [[Int]] -> [Int] -> Bool -> Int
    tsp distances begin roundTrip | (length begin) == (length distances) = cost distances begin roundTrip
                                  | otherwise = minimum [tsp distances (begin ++ [i]) roundTrip | i <- [0..(pred . length $ distances)],
                                                                              not . elem i $ begin]
    cost :: [[Int]] -> [Int] -> Bool -> Int
    cost distances [x] roundTrip = if roundTrip then ((distances!!0)!!x) else 0
    cost distances (x:y:xs) roundTrip = ((distances!!x)!!y) + (cost distances (y:xs) roundTrip)