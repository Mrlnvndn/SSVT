module Exercise1 where

import Data.List
import Mutation
import Test.QuickCheck

{-
Adding and removing items is already covered as a mutation, as well as generating a new list filled with random numbers
Some mutations which are missing are altering the order of the items in the list, swapping elements in the list,
duplicating elements in the list, reversing the list and sorting the list
-}

reverseList :: [Integer] -> [Integer]
reverseList = reverse

duplicateList :: [Integer] -> [Integer]
duplicateList [] = []
duplicateList (x : xs) = x : x : duplicateList xs

negateList :: [Integer] -> [Integer]
negateList = map negate