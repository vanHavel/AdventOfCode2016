import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State

import Utility.InputProcessing

-- data types for puzzle components
data Object = Generator Int | Chip Int deriving (Ord, Eq)

type Floor = Set Object
newtype Floors = Floors [Floor]
    
type Elevator = Int
data PuzzleState = PuzzleState {
    floors ::   Floors,
    elevator :: Elevator
}
getFloors :: PuzzleState -> [Floor]
getFloors state = let (Floors fs) = floors state in fs

-- it takes at least 2 * (size - 2) + 1 steps to move size elemets up one level. This is propagated upwards through the levels.
distanceOfItems :: PuzzleState -> Int
distanceOfItems state = (fst . cost . reverse . init . getFloors) state where 
    cost [] = (0, 0)
    cost (fl:fls) = let (s, acc) = cost fls
                        layer = acc + (Set.size fl) in 
                        if layer <= 1 then (layer, layer) else (s + (2 * (layer - 2) + 1), layer) 

-- parse a floor from a string
parseFloor :: String -> Floor
parseFloor ss = Set.fromList $ map (toFloor . splitOnSpace) $ splitOnComma ss where
    toFloor ["gen",  i] = Generator (read i)
    toFloor ["chip", i] = Chip (read i)

-- find solution to puzzle two (for puzzle one remove gen 6,7 and chip 6,7 from input)
main :: IO ()
main = do
    let file = "input/input_day11.txt"
    s <- lines <$> readFile file
    let floors = map parseFloor s
    let initialState = PuzzleState (Floors floors) 0
    let finalState = PuzzleState {
        floors = Floors ([Set.empty | i <- [1..((length floors) - 1)]] ++ [Set.unions floors]), 
        elevator = ((length floors) - 1)
    }
    print $ distanceOfItems initialState