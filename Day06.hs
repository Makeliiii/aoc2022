import Prelude
import Util
import Control.Monad (join)
import Data.List (nub)

--------------------
--   Part 1 & 2   --
--------------------
takeUntil :: Int -> Int -> String -> Int
takeUntil _ count [] = count
takeUntil num count (x:xs) = 
    if uniqueLength num (x:xs) == num then
        count
    else
        takeUntil num (count + 1) xs

uniqueLength :: Int -> String -> Int
uniqueLength count = length . nub . take count

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
    str <- readFile "inputs/Day06.txt" :: IO String
    print $ takeUntil 4 4 $ join $ lines str
    print $ takeUntil 14 14 $ join $ lines str
