module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd ( Set(..), unionSet )
import Exercise1 (manualMethod, quickMethod, generateRandomValue)

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b)= Set (a `intersect` b)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set (a \\ b)


verifIntersectionOfSameSet :: Ord a => Set a -> Set a -> Bool
verifIntersectionOfSameSet (Set a)(Set b) = all (`elem` b) a && all (`elem` a) b



main :: IO()
main = do
    rand <- generateRandomValue
    set1 <- manualMethod rand
    set2 <- generate (quickMethod rand)
    putStrLn ("Set 1 with Manual Method: "++ show set1)
    putStrLn ("Set 2 with QuickCheck Method: "++ show set2)
    putStrLn ("Union: "++ show(setUnion set1 set2))
    putStrLn ("Intersection: "++ show(setIntersection set1 set2))
    putStrLn ("Difference: "++ show(setDifference set1 set2))
    {-
        As test Properties we can feed in the same set (multiple times, with each iteration a new random Set) 
        to the different functions and look if the properties are respected. For the union of the same two Sets we should get back the exact 
        same Set. For Intersection we should again receive the exact same list back and finally, for difference, it should return an empty list.
    -}
    quickCheck $ forAll (quickMethod rand) $ \set -> verifIntersectionOfSameSet set set
