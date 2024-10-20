module Exercise1 where

import Control.Monad
import Data.List
import GHC.IO (liftIO)
import SetOrd
import System.Random
import Test.QuickCheck

-- Time Spent: 60 min
{--
Lab3 - Exercise 1: Implement random data generator for Set (defined in SetOrd)

You could do this in a multitude of ways using quickcheck, but the easiest is probably
to generate an arbitrary list and convert it to a set.

For our own implementation we generate a randomized set with a randomized length.
We arbitrarily limit the range from which numbers are picked and did not bother implementing a 'truly' random generator,
because it is not necessary for this exercise. Size is also limited, but this could be easily if required.

--}

genRandSet :: IO (Set Int)
genRandSet = do
  setSize <- randomRIO (1, 10)
  randomNumbers <- replicateM setSize (randomRIO (1, 10))
  return (list2set randomNumbers)

genQuickCheckSet :: (Ord a, Arbitrary a) => Gen (Set a)
genQuickCheckSet = list2set <$> arbitrary

main = do
  randomSet <- genRandSet
  putStrLn $ "Set generated using genRandSet: " ++ (show randomSet)

  quickCheckSet <- generate (genQuickCheckSet :: Gen (Set Int))
  putStrLn $ "Set generated using quickcheck functions: " ++ (show quickCheckSet)