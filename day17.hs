import Utility.AStar
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Base16 as Hex

-- data types for A* search
data Direction = GoUp | GoRight | GoDown | GoLeft deriving (Ord, Eq)
instance Show Direction where
    show GoUp = "U"
    show GoRight = "R"
    show GoDown = "D"
    show GoLeft = "L"
data St = St {
    x :: Int,
    y :: Int,
    history :: [Direction]
} deriving (Ord, Eq)

-- calculate successor position in given direction
succInDir :: St -> Direction -> St
succInDir pos GoUp = pos{y = (y pos) - 1, history = (history pos) ++ [GoUp]}
succInDir pos GoRight = pos{x = (x pos) + 1, history = (history pos) ++ [GoRight]}
succInDir pos GoDown = pos{y = (y pos) + 1, history = (history pos) ++ [GoDown]}
succInDir pos GoLeft = pos{x = (x pos) - 1, history = (history pos) ++ [GoLeft]}

-- check whether it is allowed to go in the given direction
admissable :: St -> Direction -> Bool
admissable pos GoUp = (y pos) /= 1
admissable pos GoRight = (x pos) /= 4
admissable pos GoDown = (y pos) /= 4
admissable pos GoLeft = (x pos) /= 1

-- get directions of open doors from current state
openDirs :: St -> [Direction]
openDirs pos = map fst . filter ((flip elem) ['b'..'f'] . snd) $ zip [GoUp, GoDown, GoLeft, GoRight] (take 4 $ hash pos)

-- manhattan distance heuristic
manhattan :: Heuristic St
manhattan pos = (4 - (x pos)) + (4 - (y pos))

-- goal test
goal :: GoalTest St
goal pos = ((x pos) == 4) && ((y pos) == 4)

-- successor function
succs :: SuccFun St
succs pos = if (x pos == 4) && (y pos == 4) 
    then Set.empty 
    else Set.fromList $ [succInDir pos dir | dir <- openDirs pos, admissable pos dir]
iteratedSuccs :: St -> [[St]]
iteratedSuccs pos = iterate (\s -> Set.toList . Set.unions . map succs $ s) [pos]

-- hash of a position in hex
hash :: St -> String
hash pos = CB.unpack . Hex.encode . MD5.hash . CB.pack $ "qtetzkpl" ++ (concat . map show $ history pos)

main :: IO ()
main = do
    let initialPos = St {x = 1, y = 1, history = []}
    let pos = snd $ genAStarSearch initialPos manhattan succs goal
    print . concat . map show $ history pos
    let reachable = takeWhile (/= []) $ iteratedSuccs initialPos
    let longest = snd . last . filter (any goal . fst) $ zip reachable [0..]
    print longest
