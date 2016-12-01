module Utility.Lists where
    
-- get first element of list appearing twice, if existing
firstRepetition :: Eq a => [a] -> Maybe a
firstRepetition [] = Nothing
firstRepetition (x:xs) | elem x xs = Just x
                       | otherwise = firstRepetition xs 