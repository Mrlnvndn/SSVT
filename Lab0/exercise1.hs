import Test.QuickCheck
import Data.List
import Data.Char

import System.Random
main :: IO()
{-
    Time Spent: 20 Minutes

    Here we are asked to use QuickSort to prove 
    that the following statements are true for *all* natural Numbers.
    Using Quicksort, we will only be able to prove the statement for 
    a certain amount of numbers that QuickSort will generate us.
    If we want to REALLY prove it for ALL natural Numbers, we will need
    to do a proof by induction.
-}

{-
    We will break up the statement in 2 seperate functions,
    the first one being the left side and the second one, the right side. 
    We will then proceed to compare them using Quickcheck and running through every single Value
-}
main = do -- All our checks are called here by main
    putStrLn "Proof for first statement: "
    quickCheck firstTest
    putStrLn "Proof for second statement: "
    quickCheck secondTest


leftSide1 :: Int -> Int -- Using Int instead of Integer because n is going to be relatively small with quickcheck
leftSide1 n = sum [x^2 | x <- [1..n]]

rightSide1 :: Int -> Int
rightSide1 n = n * (n + 1)*(2 * n+1) `div` 6 

-- We then implement the QuickCheck function making sure to only get the Natural numbers
firstTest :: Int -> Bool
firstTest n = let a = abs n in leftSide1 a == rightSide1 a


{-
    Same process for the second statement
-}

leftSide2 :: Int -> Int
leftSide2 n = sum [x^3 | x <- [1..n]]

rightSide2 :: Int -> Int
rightSide2 n = (n * (n+1)`div` 2)^2

secondTest :: Int -> Bool
secondTest n = let a = abs n in leftSide2 a == rightSide2 a

