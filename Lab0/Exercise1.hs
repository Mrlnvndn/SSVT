module Exercise1 where

import Test.QuickCheck

-- Time spent: 30 min

{-
Question:
Test the following statements with quickcheck:
    - 1**2 + 2**2 + ⋅⋅⋅ + n**2 = n(n + 1)(2n + 1)/6
    - 1**3 + 2**3 +⋅⋅⋅ +n**3 = (n(n + 1)/2)**2
Prove they are true for all natural numbers
-}

-- All natural numbers
nat = [1..]
genPos :: Gen Integer
genPos = abs `fmap` (arbitrary :: Gen Integer) `suchThat` (> 0)
genListOfPos :: Gen [Integer]
genListOfPos = listOf genPos

-- First equation
eq1a :: Integer -> Integer
eq1a n = sum [x^2 | x <- [1..n]]

eq1b :: Integral a => a -> a
eq1b n = n*(n + 1)*(2*n + 1) `div` 6

eq1 :: Integer -> Bool
eq1 n = eq1a n == eq1b n

-- Second equation

eq2a :: Integer -> Integer
eq2a n = sum [x^3 | x <- [1..n]]

eq2b :: Integer -> Integer
eq2b n = (n * (n + 1) `div` 2)^2

eq2 :: Integer -> Bool
eq2 n = eq2a n == eq2b n


main = do
    putStrLn "First equation test:"
    quickCheck $ forAll genPos eq1
    putStrLn "Second equation test:"
    quickCheck $ forAll genPos eq2
