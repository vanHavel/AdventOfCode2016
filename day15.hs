import Data.Tuple

-- the discs with size and initial position
discs :: [(Int, Int)]
discs = [(17,5),(19,8),(7,1),(13,7),(5,1),(3,0),(11,0)]

-- find solutions to puzzle 2, for first puzzle run on init discs
main :: IO ()
main = do
    let distDiscs = map (\(x, y) -> (x, x - y)) discs
        subDiscs = zipWith (\(x, y) z -> (x, (y - z) `mod` x)) distDiscs [1..]
        solution1 = snd $ foldr1 chineseFold subDiscs where
            chineseFold = \(modulus, res) (size, val) -> (modulus * size, chinese (val, size) (res, modulus))
    print solution1
    
-- formula of chinese remainder theorem        
chinese :: (Int, Int) -> (Int, Int) -> Int
chinese (c1, n1) (c2, n2) = mod (c1 * (snd euclid) * n2 + c2 * (fst euclid) * n1) (n1 * n2) where
    euclid = genEuclid n1 n2
    
-- given n1, n2 such that gcd(n1,n2) = 1, return a,b such that a * n1 + b * n2 = 1
-- this is the generalized euclidian algorithm for coprime n1, n2
genEuclid :: Int -> Int -> (Int, Int)
genEuclid n1 n2 | n1 < n2   = swap $ genEuclid n2 n1
                | otherwise = let m     = mod n1 n2
                                  d     = div n1 n2
                                  (a,b) = genEuclid n2 m in
                                      if m == 1 
                                          then (1, -d) 
                                          else (b, a - b * d)