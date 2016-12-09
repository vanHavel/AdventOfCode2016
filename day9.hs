import Utility.InputProcessing

-- find solutions to both puzzles
main :: IO ()
main = do
    let file = "input/input_day9.txt"
    s <- readFile file
    let decompressed = decompress s
    putStrLn $ show $ length decompressed
    let len = decompressedLength s
    putStrLn $ show len
    
-- decompress a given string using protocol version 1
decompress :: String -> String
decompress [] = []
decompress ('(':cs) = let (untilClosing, (')' : afterClosing)) = span (/= ')') cs
                          [len, times] = (map read $ splitOnCross untilClosing)
                          (toRepeat, rest) = splitAt len afterClosing in
                              (take (len * times) (cycle toRepeat)) ++ decompress rest
decompress (c:cs) = c:(decompress cs)

-- compute length of decompressed version of a string using protocol version 2
decompressedLength :: String -> Int
decompressedLength [] = 0
decompressedLength ('(':cs) = let (untilClosing, (')' : afterClosing)) = span (/= ')') cs
                                  [len, times] = (map read $ splitOnCross untilClosing)
                                  (toRepeat, rest) = splitAt len afterClosing in
                                      times * (decompressedLength toRepeat) + decompressedLength rest
decompressedLength (c:cs) = 1 + decompressedLength cs