import qualified Crypto.Hash.MD5 as MD5

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Base16 as Hex

import Data.List

-- a candidate for a key, stored with index, the triplet character and counter of hashes to check
data Candidate = Candidate {
    ind :: Int,
    ch :: Char,
    counter :: Int
}
-- decrease counter by one
decreaseCounter :: Candidate -> Candidate
decreaseCounter cand = cand{counter = pred $ counter cand}

-- find solutions to the puzzles
main :: IO ()
main = do
    let salt = "ihaygndm"
    let indices1 = take 64 $ pseudoSort 1000 $ keyIndices salt 1
    print $ last indices1
    let indices2 = take 64 $ pseudoSort 1000 $ keyIndices salt 2017
    print $ last indices2
    
-- sort an infinite list of numbers where the maximum displacement is known
pseudoSort :: Int -> [Int] -> [Int]
pseudoSort n l@(x:xs) = (head sortedBegin):(pseudoSort n ((tail sortedBegin) ++ (snd splitList))) where 
    splitList = span (\y -> y < x + n) l
    sortedBegin = (sort . fst) splitList

-- calculate indices of valid keys, given number for key stretching 
keyIndices :: String -> Int -> [Int]
keyIndices salt n = inds salt n 0 [] where
    -- store current hash index and key candidates 
    inds :: String ->Int -> Int -> [Candidate] -> [Int]
    inds salt n cur candidates = let curHash = stretchHash n $ CB.pack $ salt ++ (show cur)
                                     (newFound, notFound) = partition (containsFive (CB.unpack curHash)) candidates
                                     newInds = map ind newFound
                                     decCands = filter (\x -> (counter x) > 0) $ map decreaseCounter notFound 
                                     mTrip = firstTriplet (CB.unpack curHash) in
                                         case mTrip of
                                             Nothing -> newInds ++ (inds salt n (cur + 1) decCands)
                                             Just c -> newInds ++ (inds salt n (cur + 1) (decCands ++ [Candidate cur c 1000]))
                                           
-- get first occurring character triplet in string                                           
firstTriplet :: String -> Maybe Char
firstTriplet s | length s < 3 = Nothing
               | otherwise = let c1:c2:c3:rest = s in
                   if c1 == c2 && c2 == c3 then Just c1 else firstTriplet (c2:c3:rest)
                   
-- check whether string contains a length 5 sequence of a given character
containsFive :: String -> Candidate -> Bool
containsFive s cand = isInfixOf (replicate 5 $ ch cand) s

-- hash for a number of times
stretchHash :: Int -> ByteString -> ByteString
stretchHash n bs = iterate (Hex.encode .MD5.hash) bs!!n