import Data.List
import Data.Maybe

import Utility.InputProcessing
import Utility.Lists

-- data structure for commands
data Command = 
    SwapPos Int Int |
    SwapLetters Char Char |
    RotateLeft Int |
    RotateRight Int |
    RotateRightBy Char |
    RotateLeftBy Char |
    Reverse Int Int |
    Move Int Int
    deriving (Show)
    
-- invert a command
invert :: Command -> Command
invert (RotateLeft i) = RotateRight i
invert (RotateRight i) = RotateLeft i
invert (RotateRightBy c) = RotateLeftBy c
invert (Move i j) = Move j i
invert cmd = cmd

-- parsing a command
parseCommand :: String -> Command
parseCommand s = let parts = splitOnSpace s in
    case parts!!0 of
        "swap"    -> case parts!!1 of
            "position" -> SwapPos     (read $ parts!!2) (read $ parts!!5)
            "letter"   -> SwapLetters (let [x] = parts!!2 in x) (let [x] = parts!!5 in x)
        "rotate"  -> case parts!!1 of
            "left"  -> RotateLeft  (read $ parts!!2)
            "right" -> RotateRight (read $ parts!!2)
            "based" -> RotateRightBy (let [x] = parts!!6 in x)
        "reverse" -> Reverse (read $ parts!!2) (read $ parts!!4)
        "move"    -> Move    (read $ parts!!2) (read $ parts!!5)

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day21.txt"
    s <- readFile file
    let commands = map parseCommand $ lines s
    let result1 = foldl runCommand "abcdefgh" commands
    print result1
    let result2 = foldr (flip runCommand) "fbgdceah" $ map invert commands
    print result2
    
-- run a command on a string
runCommand :: String -> Command -> String
runCommand s c = case c of
    SwapPos i j -> setPos i (s!!j) . setPos j (s!!i) $ s
    SwapLetters a b -> let Just i = elemIndex a s
                           Just j = elemIndex b s in
                               setPos i (s!!j) . setPos j (s!!i) $ s
    RotateLeft i -> rotateLeft i s
    RotateRight i -> rotate i s
    RotateRightBy c -> let Just i = elemIndex c s 
                           amount = 1 + i + (if i > 3 then 1 else 0) in
                               rotate amount s
    RotateLeftBy c -> rotateLeft amount s where
        n = length s
        amount = 1 + previousPos + (if previousPos > 3 then 1 else 0)
        previousPos = head [candidate | candidate <- [0..(pred n)], (fromMaybe (-1) (elemIndex c s)) == ((rotated candidate) `mod` n)]
        rotated candidate = candidate + 1 + candidate + (if candidate > 3 then 1 else 0)
    Reverse i j -> let (front, rest) = splitAt i s
                       (middle, end) = splitAt (j - i + 1) rest in
                           front ++ (reverse middle) ++ end
    Move i j -> insertAt j (s!!i) $ delete (s!!i) s
    
