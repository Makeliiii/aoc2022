import Prelude
import Util
import Control.Monad (join)
import Data.List (nub)

--------------------
--   Part 1 & 2   --
--------------------
takeUntil :: Int -> String -> Int
takeUntil count = takeUntil' count count

takeUntil' :: Int -> Int -> String -> Int
takeUntil' num count xs
    | uniqueLength num xs == num = count
    | otherwise = takeUntil' num (count + 1) (tail xs)

uniqueLength :: Int -> String -> Int
uniqueLength count = length . nub . take count

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
    str <- readFile "inputs/Day06.txt" :: IO String
    print $ takeUntil 4 $ join $ lines str
    print $ takeUntil 14 $ join $ lines str
