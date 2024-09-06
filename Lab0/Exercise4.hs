import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Excercise 3
-- Time Spent: 50 min

reversal :: Integer -> Integer
reversal = read . reverse . show

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | otherwise = isPrimeHelper n 2

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n d
  | d > (n `div` 2) = True
  | n `mod` d == 0 = False
  | otherwise = isPrimeHelper n (d + 1)

reversibleStream :: [Integer]
reversibleStream = [int | int <- [1 .. 10000], isPrime int, isPrime (reversal int)]

prop_reversalCorrectness :: Integer -> Bool
prop_reversalCorrectness n = (reversal . reversal) n == n

prop_primeReversibility :: [Integer] -> Bool
prop_primeReversibility = all (\a -> isPrime a && isPrime (reversal a))

prop_primeMembership :: [Integer] -> Bool
prop_primeMembership = all isPrime

isInList :: Integer -> [Integer] -> Bool
isInList x xs = x `elem` xs

main :: IO ()
main = do
  print reversibleStream