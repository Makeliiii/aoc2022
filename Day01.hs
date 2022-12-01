import Prelude
import Text.Read (readMaybe)
import Data.List (sort)

totalList :: [[Int]] -> [Int]
totalList = map sum

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
topThreeTotal = sum . take 3 . sortIncreasing
    where
        sortIncreasing :: [[Int]] -> [Int]
        sortIncreasing = reverse . sort . totalList

main :: IO ()
main = do
    calories <- readFile "inputs/Day01.txt" :: IO String
    let calorieList = splitWhenNothing $ readMaybeList $ lines calories
    print $ most calorieList
    print $ topThreeTotal calorieList
