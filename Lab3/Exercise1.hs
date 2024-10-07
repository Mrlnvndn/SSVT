module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd (Set(..))


{-
Random Data Generator 

Time spent: 45 minutes
-}

-- newtype Set a = Set [a] deriving (Eq,Ord) - Set as implemented in SetOrd.hs


randomSet :: IO (Set Int)
randomSet = do
    length <- randomRIO (0, 20)
    n <- randomRIO (0, 20)
    genIntSet length n

genIntSet :: Int -> Int -> IO (Set Int)
genIntSet _ 0 = return (Set [])
genIntSet length n = do
    x <- randomRIO (0, 100)
    y <- randomRIO (-x, x+1)
    Set xs <- genIntSet length (n-1)
    return (Set (y:xs))

-- Implementation using QuickCheck
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        list <- arbitrary
        return (Set (take 20 list))  -- Limit the size of the set to 20 elements, since I am doing same in manual implementation
quickCheckSetGen :: IO (Set Int)
quickCheckSetGen = 
    generate (arbitrary :: Gen (Set Int)) 

main = do
    result <- randomSet
    putStrLn $ "Manual implementation: "
    print result
    putStrLn $ "QuickCheck implementation: "
    quickCheckSetGen