    import Data.Bits 
    import Data.Set (Set)
    import qualified Data.Set as Set

    import Utility.AStar

    -- the labyrinth is an infinite list of infinite lists, first index = x, second index = y
    type Field = [[Bool]]
    -- positions in the field
    type Position = (Int, Int)
    -- check passability
    passable :: Field -> Position -> Bool
    passable m (x,y) | (x >= 0 && y >= 0) = (m!!x)!!y
                     | otherwise = False
                 
    -- heuristic: manhattan distance
    manhattan :: Heuristic Position
    manhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

    -- create the field
    maze :: Field
    maze = [[isFree x y | y <- [0..]] | x <- [0..]] where
        isFree x y = even . popCount $ ((x + y)^2 + 3 * x + y + 1352 :: Int) 
    
    -- find solutions to the puzzles
    main :: IO ()
    main = do
        let solution1 = aStarSearch (1, 1) (31, 39) manhattan posSuccs
        print solution1
        let solution2 = Set.size $ succsIn 50 (1,1)
        print solution2
                
    -- get reachable positions from a position
    posSuccs :: Position -> Set Position
    posSuccs pos@(x, y) = Set.fromList $ filter (passable maze) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    -- set of reachable positions in n steps
    succsIn :: Int -> Position -> Set Position
    succsIn 0 pos = Set.singleton pos
    succsIn n pos = let oneLess = succsIn (n - 1) pos in
        Set.foldr (\succs acc -> Set.union succs acc) oneLess (Set.map posSuccs oneLess)