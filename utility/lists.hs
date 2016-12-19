module Utility.Lists where
    
import Data.Function
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
    
-- get first element of list appearing twice, if existing
firstRepetition :: Eq a => [a] -> Maybe a
firstRepetition [] = Nothing
firstRepetition (x:xs) | elem x xs = Just x
                       | otherwise = firstRepetition xs 
                       
-- split list after every n elements
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs | (length xs) <= n = xs:[]
                | otherwise        = (take n xs):(splitEvery n $ drop n xs)
                
-- get mode (most common element) of a list
mode :: Ord a => [a] -> a
mode xs = fst $ maximumBy (compare `on` snd) (Map.toList $ countElems xs)

-- get least common element of a list
leastCommon :: Ord a => [a] -> a
leastCommon xs = fst $ minimumBy (compare `on` snd) (Map.toList $ countElems xs)

-- return unique elements of list together with their count in a map
countElems :: Ord a => [a] -> Map a Int
countElems [] = Map.empty
countElems (x : xs) = case Map.lookup x m of
                         Nothing -> Map.insert x 1 m
                         Just c  -> Map.insert x (c + 1) m
                      where m = countElems xs
                      
-- rotate a list by n steps to the right
rotate :: Int -> [a] -> [a]
rotate n [] = []
rotate 0 xs = xs
rotate n xs = rotate (n-1) $ (last xs) : (init xs)

-- rotate a list by n steps to the left
rotateLeft :: Int -> [a] -> [a]
rotateLeft n [] = []
rotateLeft 0 xs = xs
rotateLeft n xs = (drop n xs) ++ (take n xs)

-- return list of all pairs including for all elements x,y the pairs (x,x) but only one of (x,y),(y,x)
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs l@(x:xs) = (zip (repeat x) l) ++ (pairs xs)

-- drop every second element of list
dropEverySecond :: [a] -> [a]
dropEverySecond [] = []
dropEverySecond [x] = [x]
dropEverySecond (x:y:xs) = x:(dropEverySecond xs) 