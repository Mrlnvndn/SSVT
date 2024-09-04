import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 0.1
squareSum :: Integer -> Integer
squareSum n = sum [x ^ 2 | x <- [1 .. n]]

squareSum2 :: Integer -> Integer
squareSum2 n = n * (n + 1) * (2 * n + 1) `div` 6

testSquareSum :: Integer -> Bool
testSquareSum n = squareSum n == squareSum2 n

main :: IO ()
main = quickCheck $ forAll (choose (1, 1000)) testSquareSum