module Exercise1 where

import SetOrd
import Test.QuickCheck
import System.Random
import Data.List
import GHC.IO (liftIO)

-- Time Spent: 60 min
{--
Lab3 - Exercise 1: Implement random data generator for Set (defined in SetOrd) 

You could do this in a multitude of ways, but the easiest is probably to generate an arbitrary list and convert it to a set.

--}

range = (-1000, 1000)


-- Picked a range randomly that is not too small
-- (Did end up using the choose function from QuickCheck, because otherwise we cannot get a Gen)
randomInt :: Gen Int
randomInt = choose range

genRandSet:: Int -> Gen (Set Int)
genRandSet 0 = return emptySet
genRandSet n = do
    x <- randomInt
    xs <- genRandSet (n-1)
    return (insertSet x xs)

-- And finally generate a randomized set with a randomized length.
-- Note that both generators are specifically written in a way so infinite sets are possible.
-- So if we were to use those generators in later exercises we need to impose a size limitation on those generators, 
-- due to the necessity of processing those potentially very very very large sets.
-- Since with this restriction the generated sets wouldn't be truly "random", we decided to leave that aspect out in this exercise. 


instance Arbitrary (Set Int) where
    arbitrary = do
            x <- randomInt -- limiting the size for practical purposes
            genRandSet x

quickCheckSetGen :: (Ord a, Arbitrary a) => Gen (Set a)
quickCheckSetGen = list2set <$> arbitrary;

main = do
    randomSet <- generate arbitrary :: IO (Set Int)
    putStrLn (show randomSet)




