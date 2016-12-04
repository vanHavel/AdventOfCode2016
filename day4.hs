import Data.Char
import Data.List
import Data.Ord
import Data.Function

import Utility.InputProcessing

-- data type for a single room description
type Name = String
type Checksum = String
data RoomInfo = RoomInfo {
    name     :: Name,
    idNum    :: Int,
    checksum :: Checksum
} deriving Show

-- convert input line to room info
toRoomInfo :: String -> RoomInfo
toRoomInfo line = RoomInfo {
    name     = concat (init splitLine),
    idNum    = fst $ lastPart,
    checksum = snd $ lastPart
} where
    splitLine = splitOnDash line
    lastPart  = handleBrackets $ last splitLine
    handleBrackets :: String -> (Int, Checksum)
    handleBrackets s = (read $ fst splitS, init $ tail $ snd splitS) where
        splitS = span (/= '[') s

-- check whether a room info is valid    
isValid :: RoomInfo -> Bool
isValid info = (checksum info) == (calculateChecksum $ name info)

-- calculate checksum of a name
calculateChecksum :: Name -> Checksum
calculateChecksum name = take 5 sortedByOccurrence where
    occurring = sortBy (compare `on` ord) (nub name)
    sortedByOccurrence = sortBy ((comparing Down) `on` (numberOfOccurrence name)) occurring
    numberOfOccurrence :: Name -> Char -> Int
    numberOfOccurrence name = \c -> (length $ filter (==c) name)
    
-- decrypt a room name of a room description    
decrypt :: RoomInfo -> RoomInfo
decrypt (RoomInfo name idNum checksum) = RoomInfo (map (shift idNum) name) idNum checksum where
    shift :: Int -> Char -> Char
    shift i c = chr $ ((((ord c) - (ord 'a')) + i) `mod` 26) + (ord 'a')

-- find solutions to the puzzles
main :: IO ()
main = do
    let file = "input/input_day4.txt"
    s <- readFile file
    let processedInput = map toRoomInfo $ lines s
    let validRooms = filter isValid processedInput 
    let solution1 = sum $ map idNum validRooms
    putStrLn $ show solution1
    let decryptedRooms = map decrypt validRooms
    let poleRooms = filter (\room -> isInfixOf "pole" (name room)) decryptedRooms
    putStrLn $ show $ map idNum poleRooms
    

    