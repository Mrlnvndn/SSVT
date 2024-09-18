import Data.List
import System.Random
import Test.QuickCheck
-- Time spent: 90 Minutes

-- Function with edge conditions and a recursive structure
factorial :: Integer -> Integer
factorial n  
    | n == 0 = 1
    | n == 1 = 1
    | n > 0 = n * factorial (n - 1)
    | otherwise = error "Negative number"

{-
    Those Properties have been chosen because they are the essential properties for factorial numbers to get computed 
    correctly. It would have been ideal to use a much broader range to test the properties but factorials are growing
    very quickly. The best way to prove it for any given Integer input, would have been to use proof by induction.
-}

--Test Structure where Generator will input purely negative Numbers
testNegatives :: Integer -> Bool
testNegatives n = n < 0
-- Property assumes that the factorial of n is n * factorial (n+1)
propFactorialRecursive :: Integer -> Property
propFactorialRecursive n = n >= 1 ==> factorial n == n * factorial (n - 1)
-- Property states that the factorial of (n+1) will always be bigger than the factorial of n
propFactorialComparing :: Integer -> Property
propFactorialComparing n = n >= 1 ==> factorial (n+1) > factorial n
--property states that all the values <= n can divide the factorial n 
propFactorialDivisibility :: Integer -> Property
propFactorialDivisibility n = n >= 1 ==> all (\m -> factorial n `mod` m == 0) [1..n]

-- Number generator for the negative property, range doesn't really matter as ALL negative numbers should not work anyways
genNegativeInt :: Gen Integer
genNegativeInt = choose(-10000, -1)

-- This range seems to be a good range for the test of factorials to not load for too long on my laptop
genInt :: Gen Integer
genInt = choose (1, 1000)
--Calling all the quickchecks with custom Number Generator
main :: IO ()
main = do
    putStrLn "Check for factorial recursive: "
    quickCheck (forAll genInt propFactorialRecursive)
    putStrLn "Check for factorial comparing: "
    quickCheck (forAll genInt propFactorialComparing)
    putStrLn "Check for factorial divisibility: "
    quickCheck (forAll genInt propFactorialDivisibility)
    putStrLn "Check for negatives: "
    quickCheck (forAll genNegativeInt testNegatives)

