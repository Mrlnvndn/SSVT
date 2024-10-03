module Exercise2 where

import SetOrd

-- The idea for this function is:
-- Either set being empty should also return an empty set. Nothing else needs to be checked.
-- If first of setA in setB then combine with intersect of the rest of setA with setB
-- Otherwise skip first entry and recurse
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection setA setB
  | isEmpty setA || isEmpty setB = emptySet
  | xA `inSet` setB = insertSet xA (setIntersection (Set xsA) setB)
  | otherwise = setIntersection (Set xsA) setB
        where Set (xA:xsA) = setA

-- Just decided to use the one in SetOrd, because there is no point implementing the same twice.
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

-- I used the fact that the difference between two sets is the same as the difference between the
-- intersection and union of the sets
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference setA setB
    | isEmpty setA = setB
    | isEmpty setB = setA
    | otherwise = list2set $ filter (\x -> not (x `inSet` intersect)) xs
        where
            union = setUnion setA setB
            intersect = setIntersection setA setB
            Set xs = union

{--
Properties to test these three functions. Initially we were conflicted about what exactly the subject matter
of our tests should be.
First of all we need to test simple properties of set operations:
- All in result set need to be in either A or B
- Check the paths with empty Sets (edge cases)
- 

--}







