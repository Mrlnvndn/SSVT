module Exercise1 where

-- import Data.List
import System.Random
import Test.QuickCheck

{-
Exercise 1: Create a factorial function using recursion and test it using at least 2 properties of the factorial function
-}


factorial :: Integer -> Integer
factorial <0 = error "Factorial cannot be calculated for negative value"
factorial 0 = 1
factorial 1 = 1
factorial  n = n * factorial (n-1)

simpleFactorial :: Integer -> Integer
simpleFactorial n = product [1..n]

-- Generator for reasonable positive numbers
genPos :: Gen Integer;
genPos = arbitrary `suchThat` (\x -> x > 0 && x Prelude.< 10000) 

-- Properties

-- Test that base cases hold
propBaseCaseFactorial :: Bool
propBaseCaseFactorial = factorial 0 == 1 && factorial 1 == 1

-- Test against a product-base function
propPositiveFactorial :: [Integer] -> Bool
propPositiveFactorial ns = and [f n | n <- ns] where
    f x = simpleFactorial x == factorial x

-- (n-1)! = n!/n
prop1 :: Integer -> Bool
prop1 n = factorial (n-1) == factorial n `div` n

-- prop2 :: Integer -> Bool


main :: IO ()
main = do
    let resBase = if propBaseCaseFactorial then "Passed" else "Failed"
    putStrLn $ "Testing base case: " ++ resBase
    putStrLn "Test against different (basic product) version of fact \ 
            \(for random numbers between 1 and 1000):"
    quickCheck $ forAll (listOf genPos) propPositiveFactorial
    putStrLn "Test property: (n-1)! = n!/n"
    quickCheck $ forAll genPos prop1