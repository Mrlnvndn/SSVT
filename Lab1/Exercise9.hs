import Data.List
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--For a list to be a permutation of another one, it has to be of the same length and none of the elements can be at the same position of the other list
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = iterations xs ys 0 &&  length xs == length ys

--Going through my first list, checking wether the current element is part of the second list (has to be) and checking wether it is not at the same index
--If element holds true for these two conditions, then going to the following element, and iterating through the whole list.
iterations :: Eq a => [a] -> [a] -> Int -> Bool
iterations [] _ _ = True
iterations (x:xs) ys ind
    | x `elem` ys && ys!!ind /= x = iterations xs ys (ind+1)
    | otherwise = False

--If we can assume that there is no duplicates in our list then we can have the following properties to check:

--If we order two list of permutations, they should be the same List
sameList :: Ord a => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

symmetry :: Eq a => [a] -> [a] -> Bool
symmetry xs ys = isPermutation xs ys == isPermutation ys xs

main :: IO()
main = do
    --List of well chosen Lists to test
    print (isPermutation [1,2,3] [2,3,1])
    print (isPermutation [1,2,3,4] [4,3,2,1])

    --Lists that are not permutations
    print (isPermutation [1,2,3] [3,2,1])
    print (isPermutation [1,2,3] [2,2,0])

{-
    You may assume that your input lists do not contain duplicates. What does
    this mean for your testing procedure?
    This means that my permutations are easier to test but it wouldn't have altered my testing anyways

    It is very complicated to automate the testing process as quickcheck can't easily create permutations. 
-}

