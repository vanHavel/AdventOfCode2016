-- run cabal install cryptohash for this package
import qualified Crypto.Hash.MD5 as MD5

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString.Base16 as Hex

import Control.Monad.State
import Data.List
import Data.Function

-- find solutions to puzzles one and two
main :: IO ()
main = do
    let input = "cxdnnyjw"
    let hashes = evalState (interestingHashes (CB.pack input)) 0
    let decodedHashes = map (CB.unpack . Hex.encode) hashes
    let password1 = map (!!5) $ take 8 decodedHashes
    putStrLn password1
    let hashesNeeded = takeUntilPositionsCovered decodedHashes
    let sortedHashes = sortBy (compare `on` (!!5)) hashesNeeded
    let password2 = map (!!6) sortedHashes
    putStrLn password2
    
-- take from infinite list of strings until all positions of the password are covered, taking only the first match for each position
takeUntilPositionsCovered :: [String] -> [String] 
takeUntilPositionsCovered ss = tupc ss "01234567" where
    tupc _ [] = []
    tupc (s:ss) xs | elem pos xs = (s : (tupc ss (delete pos xs)))
                   | otherwise   = tupc ss xs
                   where pos = (s!!5)
    
-- get all hashes that start with 00000 in hex
interestingHashes :: ByteString -> State Int [ByteString]    
interestingHashes input = do
    next <- hashTillZeros input
    rest <- interestingHashes input
    return $ next : rest
  
-- hash input with counter until hash value starts with 00000 in hex  
hashTillZeros :: ByteString -> State Int ByteString
hashTillZeros input = do
    count <- get
    let nextHash = MD5.hash (B.append input (CB.pack $ show count))
    let nextHashStart = take 5 $ CB.unpack $ Hex.encode nextHash
    modify (+1)
    if nextHashStart == "00000" 
        then return nextHash
        else hashTillZeros input