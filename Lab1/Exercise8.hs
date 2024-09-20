import Data.List
import Test.QuickCheck
import Lecture3


-- function to find prime numbers
isPrime :: Integer -> Bool
isPrime i
    | i < 2 = False
    | otherwise =
        let len = length [k | k <- [1..floor $ sqrt $ fromIntegral i], mod i k == 0]
        in len == 1

-- primeList :: [Integer]
-- primeList =
--     [k | k <- [2..50], isPrime k]

multiplyAddList :: [Integer] -> Integer
multiplyAddList i =
    product i + 1

removeRepeats :: [[Integer]] -> [[Integer]]
removeRepeats [] = []
removeRepeats (x:xs)
    | x `elem` xs = removeRepeats xs
    | otherwise = x : removeRepeats xs


genLists :: Integer -> [[Integer]]
genLists 0 = []
genLists n = removeRepeats ([k | k <- [2..n], isPrime k] : genLists (n - 1))


-- ex input [2,3,5,7]
counterexamples :: [([Integer], Integer)]
counterexamples =
    let counters = [k | k <- genLists 30, not (isPrime $ multiplyAddList k)]
    in [(k, multiplyAddList k) | k <- counters]