-- input string
input :: [Bool]
input = map (\x -> if x == '1' then True else False) "10001001100000001"

-- find solutions to the puzzles
main :: IO ()
main = do
    let dat1 = makeData 272 input
    let chk1 = checksum dat1
    putStrLn . concat . map (\x -> if x == True then "1" else "0") $ chk1
    let dat2 = makeData 35651584 input
    let chk2 = checksum dat2
    putStrLn . concat . map (\x -> if x == True then "1" else "0") $ chk2
    
-- make data sequence from input
makeData :: Int -> [Bool] -> [Bool]
makeData n a | length a >= n = take n a
             | otherwise = makeData n (a ++ (False : (map not . reverse $ a))) 

-- calculate checksum of data
checksum :: [Bool] -> [Bool]
checksum dat | odd $ length dat = dat
             | otherwise = checksum $ map (\[x, y] -> x == y) $ pairs dat where
                 pairs [] = []
                 pairs (x:y:xs) = [x, y]:(pairs xs)