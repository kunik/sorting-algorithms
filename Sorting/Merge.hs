module Sorting.Merge
(
    sort 
) where

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = sortBoth $ splitAt ((length xs) `div` 2) xs
    where
        sortBoth (x,y) = joinOrdered (sort x, sort y)
        joinOrdered (x,y)
            | x < y     = x ++ y
            | otherwise = y ++ x
