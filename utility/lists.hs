module Utility.Lists where
    
-- get first element of list appearing twice, if existing
firstRepetition :: Eq a => [a] -> Maybe a
firstRepetition xs = frs xs [] where
    frs :: Eq a => [a] -> [a] -> Maybe a
    frs [] _ = Nothing
    frs (x:xs) storage | elem x storage = Just x
                       | otherwise      = frs xs (x:storage)