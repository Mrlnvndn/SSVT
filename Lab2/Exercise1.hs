import Data.List
import Test.QuickCheck
import Mutation
import System.Random
import Control.Monad (liftM2)



{-
Outputs not covered
1. Changing the order of the list
    This output is not covered in the given mutators. This mutator would be 
    fairly weak since the only way it would not get caught is if the random
    shuffled order happened to be the same as the original.

2. Adding elements to the middle of the list
    The given mutators only added elements to the beginning or end of the list.
    This mutator is very weak since it will get caught if you are testing
    the length property of a list, meaning if you are testing that when an
    element is added, the length increases by 1.

3. Changing one random element in the list 
    Also was not covered in given mutators. This is also very weak if you are
    testing the equality property of lists. Meaning if a list has an element changed,
    it should not be equal to the original.

4. Changing the signs of everything in the list 
    If a list is 

These mutators, including the ones given in Mutation.hs 
-}

-- 1. Changing the order of the list

-- Fisher-Yates shuffle algorithm 
-- Grabbed from https://codereview.stackexchange.com/questions/232894/fisher-yates-shuffle-in-haskell
pick :: Int -> [a] -> ([a],[a])
pick _ []     = ([],[])
pick 0 (x:xs) = ([x],xs)
pick i (x:xs) = (,) <$> fst <*> ((x:) . snd) $ pick (i-1) xs

shuffle' :: [a] -> IO [a]
shuffle' = runner =<< length
          where
          runner :: Int -> [a] -> IO [a]
          runner 0 xs = return xs
          runner i xs = randomRIO (0, i-1) >>= \r -> let (y,ys) = pick r xs
                                                     in (y ++) <$> runner (i-1) ys


-- 2. Adding elements to the middle of the list
addToMiddle :: Int -> [Int] -> Gen [Int]
addToMiddle i x = return $
    let index = length x `div` 2
        firstHalf = take index x
        secondHalf = drop index x
    in firstHalf ++ [i] ++ secondHalf

-- 3. Changing one random element in the list 
-- Function to generate a random number in a given range   
addToRandomIndex :: [Int] -> Gen [Int]
addToRandomIndex x = do
    index <- choose (0, length x - 1)
    let oldNum = x !! index
    newNum <- choose (oldNum + 1, oldNum + 100)
    let firstHalf = take index x
        secondHalf = drop index x
    return $ firstHalf ++ [newNum] ++ secondHalf

-- 4. Changing the signs of everything in the list 
negateList :: [Int] -> Gen [Int]
negateList x = return $ map negate x

main = do
    result <- generate (addToRandomIndex [1, 2, 3])
    print result






