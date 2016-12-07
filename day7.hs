import Data.List

-- find solutions to puzzles 1 and 2
main :: IO ()
main = do
    let file = "input/input_day7.txt"
    s <- readFile file
    let solution1 = length (filter validABBA $ lines s)
    putStrLn $ show solution1
    let solution2 = length (filter validABApair $ lines s)
    putStrLn $ show solution2
    
-- check whether string has valid abba
validABBA :: String -> Bool
validABBA xs = (abbaOutside xs) && (not $ abbaInside $ dropWhile (/= '[') xs)

-- check that string contains abba outside of brackets
abbaOutside :: String -> Bool
abbaOutside xs | length xs < 4 = False
               | otherwise     = case xs of
                   ('[':ys)     -> abbaOutside (dropWhile (/= ']') ys)
                   (c:d:e:f:ys) -> if (c /= d && c == f && d == e) then True else abbaOutside (d:e:f:ys)
                   
-- check that string contains abba inside of brackets
abbaInside :: String -> Bool
abbaInside xs | length xs < 4 = False
              | otherwise     = case xs of
                  (']':ys)     -> abbaInside (dropWhile (/= '[') ys)
                  (c:d:e:f:ys) -> if (c /= d && c == f && d == e) then True else abbaInside (d:e:f:ys)

-- check that string contains valid aba bab pair                  
validABApair :: String -> Bool
validABApair s = let abas = getABAs s
                     babs = getBABs $ dropWhile (/= '[') s 
                     invert [b,a,b2] = [a,b,a]
                     babsInverted = map invert babs in
                         intersect abas babsInverted /= []
                         
-- get abas outside of brackets
getABAs :: String -> [String]
getABAs xs | length xs < 3 = []
           | otherwise     = case xs of
               ('[':ys)     -> getABAs (dropWhile (/= ']') ys)
               (c:d:e:ys) -> if (c /= d && c == e) then ([c,d,e]:(getABAs (d:e:ys))) else getABAs (d:e:ys)
               
-- get babs inside of brackets
getBABs :: String -> [String]
getBABs xs | length xs < 3 = []
           | otherwise     = case xs of
               (']':ys)     -> getBABs (dropWhile (/= '[') ys)
               (c:d:e:ys) -> if (c /= d && c == e) then ([c,d,e]:(getBABs (d:e:ys))) else getBABs (d:e:ys)
