module Exercise2 where

import Test.QuickCheck
import System.Random
import Data.List
import Lab0
import Control.Monad

-- Time spent: 300 min

{-
Exercise 2: Test whether function `props` actually results in randomly distributed floats over range [0..1]

The strategy was to use the probs function to generate 10000 'random' number, devide them
into 4 equal sections using the 'getSubsection' function. Afterwards we count up the length
for each of the sections and check whether it is close enough to the expected value: 10000/4.

We decided to use 5% error as an acceptable bound, as this is often used as a reasonable value
to be confident in the randomness of the distribution.

The test is performed 100 times in the main function, each with 10000 random numbers
We generally found that all 100 the tests passed, which of course doesn't prove anything definitively, but
gives a good indication of correctness.

For this exercise we do not need QuickCheck as probs itself should make sure all the test cases
are independent and random.

This exercise took a long time as we all struggled to wrap our head around the right syntax when using
the IO monad.
-}

getSubsection :: [Float] -> Float -> Float -> Int
getSubsection l lbound ubound = length sub
    where sub = [x | x <- l, x < ubound && x >= lbound]



testProps :: Int -> IO Bool
testProps n = do
    res <- probs n
    let sub1 = getSubsection res 0.0 0.25
    let sub2 = getSubsection res 0.25 0.5
    let sub3 = getSubsection res 0.5 0.75
    let sub4 = getSubsection res 0.75 1
    -- putStrLn $ "Counts: " ++ show sub1 ++ ", " ++ show sub2 ++ ", " ++ show sub3 ++ ", " ++ show sub4

    let expected = n `div` 4
    let errSum = sum [abs (expected - x) ^ 2 | x <- [sub1, sub2, sub3, sub4]]
    let stdDiv = sqrt $ fromIntegral errSum / fromIntegral n
    -- putStrLn $ "Standard deviation for current test: " ++ show stdDiv

    if stdDiv < fromIntegral n * 0.05 then 
        return True
    else return False

-- main :: IO ()
main = do
    results <- replicateM 100 (testProps 10000)
    mapM_ (putStrLn . ("Test within standard deviation: " ++) . show) results
    putStrLn "Tests finished"

