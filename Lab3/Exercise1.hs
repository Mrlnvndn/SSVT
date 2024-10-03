module Exercise1 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{-
    Time spent: 40 minutes
    Implement a random data generator for the datatype Set Int , where Set is as defined in
    SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test
    this datatype.
-}

-- Generate a random integer in a specific range to determine the amount of elements to be put in a set. Range is determined
-- by visibility when testing the code. Keeping it concise
generateRandomValue :: IO Int
generateRandomValue = randomRIO (0, 10)


-- Function to generate a list of random integers, range is also determined by visibility
generateRandomList :: Int -> IO [Int]
generateRandomList 0 = return []
generateRandomList n = do
    x <- randomRIO (0, 10)
    xs <- generateRandomList (n - 1)
    return (x:xs)

-- Function to generate a list of random integers using QuickCheck, same thing for range of values
generateRandomList' :: Int -> Gen [Int]
generateRandomList' 0 = return []
generateRandomList' n = do
    x <- choose (0, 10)
    xs <- generateRandomList' (n - 1)
    return (x:xs)

-- Function to generate a random Set of integers with "manual" method
manualMethod :: Int -> IO (Set Int)
manualMethod n = do
    values <- generateRandomList n
    return (list2set values)

-- Function to generate a random Set of integers with quickCheck library
quickMethod :: Int -> Gen (Set Int)
quickMethod n = do
    values <- generateRandomList' n
    return (list2set values)

-- converts set to a list, removes duplicates (if there are any) and compares it to the length of the original list 
manualCheckOfDups :: Set Int -> Bool
manualCheckOfDups (Set xs) = length xs == length (nub xs)

main :: IO()
main = do
    amountOfElems <- generateRandomValue
    set1 <- manualMethod amountOfElems 
    set2 <- generate (quickMethod amountOfElems)
    putStrLn (show set1)
    print set2
