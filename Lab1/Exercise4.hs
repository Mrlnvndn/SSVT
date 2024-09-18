import Data.List
import Test.QuickCheck

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = 
    length xs == length ys  -- Make sure lists are equal length
    && all (uncurry (/=)) (zip xs ys) -- Puts the lists in tuple pair form, then applies /= to all 
    && null (xs \\ ys)                

deran:: Int -> [[Int]]
deran n =
    let perms = permutations [0..n-1]
    in [k | k <- perms, isDerangement [0..n-1] k ]

{-
Testing Properties 
1. 
-}


    