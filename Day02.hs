import Prelude
import Util

--------------------
--     Part 1     --
--------------------
encode :: Char -> Int
encode x
    | x == 'A' || x == 'X' = 1
    | x == 'B' || x == 'Y' = 2
    | x == 'C' || x == 'Z' = 3

score :: Int -> Int -> Int
score x y = ((y - x + 1) `mod` 3) * 3

match :: Char -> Char -> Int
match = finalScore
    where
        finalScore :: Char -> Char -> Int
        finalScore x y = score (encode x) (encode y) + encode y

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
   rpcs <- readFile "inputs/Day02.txt" :: IO String
   let chunks = map (\x -> (head x, last x)) $ lines rpcs
   print $ sum $ map (uncurry match) chunks
