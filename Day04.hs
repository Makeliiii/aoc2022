import Prelude
import Util
import Data.Bifunctor (Bifunctor(bimap))

pairOn :: Char -> String -> (String, String)
pairOn = pairer []
    where
        pairer :: String -> Char -> String -> (String, String)
        pairer zs y (x:xs) =
            if x == y then
                (reverse zs, xs)
            else
                pairer (x:zs) y xs

pairToInt :: ((String, String), (String, String)) -> ((Int, Int), (Int, Int))
pairToInt ((x, y), (z, w)) = ((read x, read y), (read z, read w))

--------------------
--     Part 1     --
--------------------
includes :: ((Int, Int), (Int, Int)) -> Bool
includes ((x, y), (z, w)) = x <= z && y >= w || z <= x && w >= y

--------------------
--     Part 2     --
--------------------
overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((x, y), (z, w)) = z <= y && x <= w

--------------------
--      Main      --
--------------------
main :: IO ()
main = do
    pairs <- readFile "inputs/Day04.txt" :: IO String
    print $ sum $ map (fromEnum . includes . pairToInt . (\(x, y) -> (pairOn '-' x, pairOn '-' y)) . pairOn ',') $ lines pairs
    print $ sum $ map (fromEnum . overlaps . pairToInt . (\(x, y) -> (pairOn '-' x, pairOn '-' y)) . pairOn ',') $ lines pairs
