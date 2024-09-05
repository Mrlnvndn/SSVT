import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 1
-- Time Spent: 35 min
squareSum1 :: Integer -> Integer
squareSum1 n = sum [x ^ 2 | x <- [1 .. n]]

squareSum2 :: Integer -> Integer
squareSum2 n = n * (n + 1) * (2 * n + 1) `div` 6

testSquareSum :: Integer -> Bool
testSquareSum n = squareSum1 n == squareSum2 n

main :: IO ()
main = quickCheck $ forAll (choose (1, 1000)) testSquareSum