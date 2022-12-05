import Prelude
import Util
import Data.List (sort, intersect)
import Data.Char (ord, isUpper)

--------------------
--     Part 1     --
--------------------
splitRucksack :: String -> (String, String)
splitRucksack xs = (take tLenght xs, take tLenght $ reverse xs)
    where
        tLenght = length xs `div` 2

search :: String -> String -> Char
search xs = head . intersect xs

index :: Char -> Int
index c
    | isUpper c = ord c - ord 'A' + 27
    | otherwise = ord c - ord 'a' + 1

--------------------
--     Part 2     --
--------------------
splitToThree :: [String] -> [(String, String, String)]
splitToThree [] = []
splitToThree (x:y:z:xs) = (x, y, z) : splitToThree xs

searchFromThree :: String -> String -> String -> Char
searchFromThree xs ys = head . intersect xs . intersect ys

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
   rucksack <- readFile "inputs/Day03.txt" :: IO String
   let chunks = lines rucksack
   print $ sum $ map (index . uncurry search . splitRucksack) chunks
   print $ sum $ map (index . uncurry3 searchFromThree) $ splitToThree chunks
