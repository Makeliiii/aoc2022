import Prelude
import Text.Read (readMaybe)
import Data.List (sort)

total :: [Int] -> Int
total = sum

totalList :: [[Int]] -> [Int]
totalList = map total

most :: [[Int]] -> Int
most = maximum . totalList

readMaybeList :: [String] -> [Maybe Int]
readMaybeList = map readMaybe

splitWhenNothing :: [Maybe Int] -> [[Int]]
splitWhenNothing xs = pusher xs []
    where
        pusher :: [Maybe Int] -> [Int] -> [[Int]]
        pusher [] _ = []
        pusher (Nothing:xs) ys = ys : pusher xs []
        pusher (Just x:xs) ys = pusher xs (x : ys)

topThreeTotal :: [[Int]] -> Int
topThreeTotal = getTopThree . sortIncreasing
    where
        sortIncreasing :: [[Int]] -> [Int]
        sortIncreasing = reverse . sort . totalList
        getTopThree :: [Int] -> Int
        getTopThree (x:y:z:xs) = x + y + z

main :: IO ()
main = do
    calories <- readFile "inputs/Day01.txt" :: IO String
    let calorieList = splitWhenNothing $ readMaybeList $ lines calories
    print $ most calorieList
    print $ topThreeTotal calorieList
