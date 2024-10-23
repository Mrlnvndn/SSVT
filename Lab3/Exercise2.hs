module Lab3.Exercise2 where

import Data.List
import Data.Semigroup (diff)
import Lab3.Exercise1 (genQuickCheckSet, genRandSet)
import Lab3.SetOrd
import System.Random
import Test.QuickCheck

{-
For the definitions of the set functions, we extract the list from the Set, use the existing list operators in Haskell
and then transform it back into a set using list2set

We came up with one general property, which should hold for all three set function namely that 1. no duplicates shoudld exists after the function

For the intersect function whe want 1. the resulting set to be equal or smaller in size and 2. every element in the resulting set should exist
in the two original sets

For union the resulting set should 1. be equal or larger and 2. all elements from the original two sets should exist in the resulting set

For set difference the resulting set should 1. be equal to or smaller than the first original set and 2. all elements in the resulting set should
not exist in the second original set

We tested each property for each function using both quickcheck and our own implementation
-}

-- Time spent: 120 min

-- A ∩ B
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = list2set (xs `intersect` ys)

-- A ∪ B
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion = unionSet

-- A - B
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = list2set (xs \\ ys)

-- General property which should hold for all outputted sets
prop_noDuplicates :: (Eq a) => Set a -> Bool
prop_noDuplicates (Set xs) = length (nub xs) == length xs

-- Properties which should hold for setIntersection
prop_equalOrSmaller :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_equalOrSmaller (Set xs) (Set ys) (Set output) = length output <= length xs && length output <= length ys

prop_elementsInBothSets :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_elementsInBothSets (Set xs) (Set ys) (Set output) = all (\e -> e `elem` xs && e `elem` ys) output

-- Properties which should hold for setUnion
prop_equalOrLarger :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_equalOrLarger (Set xs) (Set ys) (Set output) = length output >= length xs || length output >= length ys

prop_containAllElements :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_containAllElements (Set xs) (Set ys) (Set output) = all (\e -> e `elem` xs || e `elem` ys) output

-- Properties which should hold for setDifference
prop_equalOrSmaller2 :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_equalOrSmaller2 (Set xs) (Set ys) (Set output) = length output <= length xs

prop_elementsNotInSecondSet :: (Eq a) => Set a -> Set a -> Set a -> Bool
prop_elementsNotInSecondSet (Set xs) (Set ys) (Set output) = all (`notElem` ys) output

-- List of properties
intersectProperties :: [(String, Set Int -> Set Int -> Set Int -> Bool)]
intersectProperties =
  [ ("Intersection: No duplicates: ", \s1 s2 s3 -> prop_noDuplicates s3),
    ("Intersection: Equal or smaller: ", prop_equalOrSmaller),
    ("Intersection: Elements in both sets: ", prop_elementsInBothSets)
  ]

unionProperties :: [(String, Set Int -> Set Int -> Set Int -> Bool)]
unionProperties =
  [ ("Union: No duplicates: ", \s1 s2 s3 -> prop_noDuplicates s3),
    ("Union: Equal or larger: ", prop_equalOrLarger),
    ("Union: Contain all elements: ", prop_containAllElements)
  ]

differenceProperties :: [(String, Set Int -> Set Int -> Set Int -> Bool)]
differenceProperties =
  [ ("Difference: No duplicates: ", \s1 s2 s3 -> prop_noDuplicates s3),
    ("Difference: Equal or smaller: ", prop_equalOrSmaller2),
    ("Difference: Elements not in second set: ", prop_elementsNotInSecondSet)
  ]

-- Function to print properties
testUsingCustomGenerator :: IO ()
testUsingCustomGenerator = do
  set1 <- genRandSet
  set2 <- genRandSet
  let difference = setDifference set1 set2
  let union = setUnion set1 set2
  let intersect = setIntersection set1 set2
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 intersect))) intersectProperties
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 difference))) differenceProperties
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 union))) unionProperties

-- Function to run QuickCheck tests
testUsingQuickCheckGenerator :: IO ()
testUsingQuickCheckGenerator = do
  set1 <- generate genQuickCheckSet
  set2 <- generate genQuickCheckSet
  mapM_
    ( \(desc, prop) -> do
        putStr desc
        quickCheck (prop set1 set2 (setIntersection set1 set2))
    )
    intersectProperties
  mapM_
    ( \(desc, prop) -> do
        putStr desc
        quickCheck (prop set1 set2 (setDifference set1 set2))
    )
    differenceProperties
  mapM_
    ( \(desc, prop) -> do
        putStr desc
        quickCheck (prop set1 set2 (setUnion set1 set2))
    )
    unionProperties

main :: IO ()
main = do
  testUsingCustomGenerator
  putStrLn "*****************************************************"
  testUsingQuickCheckGenerator