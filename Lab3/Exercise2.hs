import Data.List
import System.Random
import Test.QuickCheck
import SetOrd (Set(..), inSet, list2set, unionSet)
import Exercise1 (quickCheckSetGen, randomSet)


-- Convert Set to List
setToList :: Eq a => Set a -> [a]
setToList (Set xs) = xs

-- Implementations
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection s1 s2 = 
    list2set [k | k <- setToList s1, inSet k s2]

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference s1 s2 = 
    let first = [k | k <- setToList s1, not (inSet k s2)]
        second = [k | k <- setToList s2, not (inSet k s1)]
    in list2set (first ++ second)

-- Testing
manualTest :: Ord a => Set a -> Set a -> Bool
manualTest s1 s2 = do
    s1 <- randomSet 
    s2 <- randomSet
    setIntersection s1 s2
    setUnion s1 s2
    setDifference s1 s2


    