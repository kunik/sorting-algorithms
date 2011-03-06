module Sorting.QuickSort
(
    sort 
) where

sort :: (Ord a) => [a] -> [a]
sort [] = []