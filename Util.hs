module Util where

import Prelude

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn = splitter []
    where
        splitter :: Eq a => [a] -> [a] -> [a] -> [[a]]
        splitter _ _ [] = []
        splitter zs ys (x:xs) =
            if [x] == ys then
                zs : splitter [] ys xs
            else
                splitter (x:zs) ys xs

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
