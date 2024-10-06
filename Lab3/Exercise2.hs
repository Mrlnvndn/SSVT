import Data.List
import Data.Semigroup (diff)
import Exercise1 (generator, generator')
import SetOrd
import System.Random
import Test.QuickCheck

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
  set1 <- generator
  set2 <- generator
  let difference = setDifference set1 set2
  let union = setUnion set1 set2
  let intersect = setIntersection set1 set2
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 intersect))) intersectProperties
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 difference))) differenceProperties
  mapM_ (\(desc, prop) -> putStrLn (desc ++ show (prop set1 set2 union))) unionProperties

-- Function to run QuickCheck tests
testUsingQuickCheckGenerator :: IO ()
testUsingQuickCheckGenerator = do
  set1 <- generate generator'
  set2 <- generate generator'
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