import Data.List 

import Utility.InputProcessing

-- find solutions to the puzzles
main :: IO ()
main = do
    let file = "input/input_day20.txt"
    bounds <- nub <$> map parseBound <$> lines <$> readFile file
    print $ lowestIP bounds 0
    print $ allowedIPs bounds
    
-- parse IP bounds from a string
parseBound :: String -> (Int, Int)
parseBound s = (read lower, read upper) where [lower, upper] = splitOnDash s 

-- lowest IP given bounds and starting IP
lowestIP :: [(Int, Int)] -> Int -> Int
lowestIP xs cur | step xs cur == cur = cur
                | otherwise = lowestIP xs $ step xs cur
                where step [] cur = cur
                      step ((lower, upper):xs) cur | lower <= cur && upper >= cur = step xs (upper + 1)
                                                   | otherwise = step xs cur

-- get number of allowed IPs                                                   
allowedIPs :: [(Int, Int)] -> Int
allowedIPs xs = pred . length . takeWhile (< 2 ^ 32) $ iterate (\cur -> lowestIP xs (succ cur)) 0