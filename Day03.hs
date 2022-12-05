import Prelude
import Util
import Data.List (sort)
import Distribution.Simple.Utils (safeHead)
import Data.Set (fromList, intersection, toList)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY"

splitRucksack :: String -> (String, String)
splitRucksack xs = (take tLenght xs, take tLenght $ reverse xs)
    where
        tLenght = length xs `div` 2

--------------------
--     Part 1     --
--------------------
search :: String -> String -> Maybe Char
search xs ys = safeHead $ toList $ intersection a b
    where
        a = fromList xs
        b = fromList ys

index :: Maybe Char -> Int
index Nothing  = 0
index (Just x) = calc alphabet 1
    where
        calc :: String -> Int -> Int
        calc [] count = count
        calc (y:ys) count =
            if x == y then
                count
            else
                calc ys (count + 1)

--------------------
--     Part 2     --
--------------------
splitToThree :: [String] -> [(String, String, String)]
splitToThree [] = []
splitToThree (x:y:z:xs) = (x, y, z) : splitToThree xs

searchFromThree :: String -> String -> String -> Maybe Char
searchFromThree xs ys zs = safeHead $ toList $ intersection a $ intersection b c
    where
        a = fromList xs
        b = fromList ys
        c = fromList zs

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
   rucksack <- readFile "inputs/Day03.txt" :: IO String
   let chunks = lines rucksack
   print $ sum $ map (index . uncurry search . splitRucksack) chunks
   print $ sum $ map (index . uncurry3 searchFromThree) $ splitToThree chunks
