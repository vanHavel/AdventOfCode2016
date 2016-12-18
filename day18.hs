-- first row of maze
row1 :: [Bool]
row1 = map (== '.') "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."

-- the infinite maze
field :: [[Bool]]
field = iterate nextrow row1

-- function to generate the next row
nextrow :: [Bool] -> [Bool]
nextrow = nr True where
    nr :: Bool -> [Bool] -> [Bool]
    nr x [y] = [safe x y True]
    nr x (y:z:xs) = (safe x y z):(nr y (z:xs))
    
-- check whether tile is safe, given the three relevant tiles
safe :: Bool -> Bool -> Bool -> Bool
safe x y z = (x == y && y == z) || (x == z && x /= y)

-- find solutions to the puzzles
main :: IO ()
main = do
    let solution1 = length . filter (== True) . concat . take 40 $ field
    print solution1
    let solution2 = length . filter (== True) . concat . take 400000 $ field
    print solution2