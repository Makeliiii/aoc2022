import Prelude
import Data.List (sort)
import Util

--------------------
--     Part 1     --
--------------------
totalList :: [[Int]] -> [Int]
totalList = map sum

most :: [[Int]] -> Int
most = maximum . totalList

--------------------
--     Part 2     --
--------------------
topThreeTotal :: [[Int]] -> Int
topThreeTotal = sum . take 3 . sortIncreasing
    where
        sortIncreasing :: [[Int]] -> [Int]
        sortIncreasing = reverse . sort . totalList

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
    calories <- readFile "inputs/Day01.txt" :: IO String
    let calorieList = map (map read) $ splitOn [""] $ lines calories
    print $ most calorieList
    print $ topThreeTotal calorieList
