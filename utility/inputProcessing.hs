module Utility.InputProcessing where

dropElem :: Eq a => a -> [a] -> [a]
dropElem x = filter (/= x)

dropSpaces :: String -> String
dropSpaces = dropElem ' '

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x l@(y:ys) | x == y = splitOn x ys
                   | otherwise = (takeWhile (/= x) l) : (splitOn x $ dropWhile (/= x) l)

splitOnComma :: String -> [String]
splitOnComma = splitOn ','