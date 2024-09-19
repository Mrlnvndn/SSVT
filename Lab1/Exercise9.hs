import Data.List
import System.Random
import Test.QuickCheck


--Time Spent: 60 Minutes
-- Implying code from snippet from lecture

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--For a list to be a permutation of another one, it has to be of the same length and none of the elements can be at the same position of the other list
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = iterations xs ys  && ys/=xs && length xs == length ys

--Going through my first list, checking wether the current element is part of the second list (has to be) and checking wether it is not at the same index
--If element holds true for these two conditions, then going to the following element, and iterating through the whole list.
iterations :: Eq a => [a] -> [a] -> Bool
iterations xs ys = all (`elem` ys) xs

--If we can assume that there is no duplicates in our list then we can have the following properties to check:

--Property states that 2 permutations of the same list, if ordered, will return the same List
sameList :: Ord a => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

--Property states that permutations are symmetric, permutation of a list, then the list is also a permutation of the previous permutation
symmetry :: Eq a => [a] -> [a] -> Bool
symmetry xs ys = isPermutation xs ys == isPermutation ys xs


-- Generate a random list of 9 integers
randomList :: IO [Int]
randomList = sequence [randomIO | _ <- [1..9]]

randomPermGen :: [Int] -> [[Int]]
randomPermGen xs = permutations xs
 




main :: IO()
main = do
    --List of well chosen Lists to test
   randomList' <- randomList
   print randomList'
   let randomPerm = randomPermGen randomList'
   print randomPerm

{-
    You may assume that your input lists do not contain duplicates. What does
    this mean for your testing procedure?

    This means that my permutations are easier to test but it wouldn't have altered my testing anyways

    It is very complicated to automate the testing process as quickcheck can't easily create permutations. 
-}

