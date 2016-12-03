module Utility.Lists where
    
-- get first element of list appearing twice, if existing
firstRepetition :: Eq a => [a] -> Maybe a
firstRepetition [] = Nothing
firstRepetition (x:xs) | elem x xs = Just x
                       | otherwise = firstRepetition xs 
                       
-- split list after every n elements
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs | (length xs) <= n = xs:[]
                | otherwise        = (take n xs):(splitEvery n $ drop n xs)