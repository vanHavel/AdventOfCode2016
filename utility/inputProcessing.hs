module Utility.InputProcessing where

-- drop all occurrences of element out of a list
dropElem :: Eq a => a -> [a] -> [a]
dropElem x = filter (/= x)

-- drop spaces from a string
dropSpaces :: String -> String
dropSpaces = dropElem ' '

-- split list on all occurrences of elements
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x l@(y:ys) | x == y = splitOn x ys
                   | otherwise = (takeWhile (/= x) l) : (splitOn x $ dropWhile (/= x) l)

-- split string on comma
splitOnComma :: String -> [String]
splitOnComma = splitOn ','