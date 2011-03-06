module Sorting.Bubble
(
    sort 
) where

data Flip a = Yes a | No

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort xs =
   iterate swapPass xs !! (length xs - 1)
   where
      swapPass [x] = [x]
      swapPass (x:y:zs)
         | x > y     = y : swapPass (x:zs)
         | otherwise = x : swapPass (y:zs)