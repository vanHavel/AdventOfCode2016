import Utility.InputProcessing
import Utility.Lists

-- node data type
data Node = Node {
    size :: Int, 
    free :: Int
} deriving (Show, Ord, Eq)

-- utility functions on nodes
filled :: Node -> Int
filled n = (size n) - (free n)
empty :: Node -> Bool
empty n = (free n) == (size n)

-- field size
maxX = 36
maxY = 27

-- parse a Node from string
parseNode :: String -> Node
parseNode s = let parts = splitOnSpace s in
    Node {
        size = (read . init $ parts!!1),
        free = (read . init $ parts!!3)
    }

-- find solutions to part 1. part 2 is easily solved by hand (242)
main :: IO ()
main = do
    let file = "input/input_day22.txt"
    s <- readFile file
    let field = splitEvery (maxY + 1) . map parseNode . drop 2 . lines $ s
    let viablePairs = [((i1, j1), (i2, j2)) | i1 <- [0..maxX], j1 <- [0..maxY], i2 <- [0..maxX], j2 <- [0..maxY], 
            (i1 /= i2) || (j1 /= j2), not . empty $ (field!!i2)!!j2, free ((field!!i1)!!j1) >= filled ((field!!i2)!!j2)]
    print $ length viablePairs