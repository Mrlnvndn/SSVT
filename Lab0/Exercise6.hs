import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 1
-- Time Spent: 100 min

-- Helper methods
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

primes = 2 : [x | x <- [3 ..], all (\y -> x `mod` y /= 0) (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)]

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x : xs) = x + sumList xs

consecutive101Prime :: Integer
consecutive101Prime = findPrimeSum 0
  where
    findPrimeSum :: Int -> Integer
    findPrimeSum x
      | isPrime sumOfPrimes = sumOfPrimes
      | otherwise = findPrimeSum (x + 1)
      where
        primeList = take 101 (drop x primes)
        sumOfPrimes = sumList primeList

main :: IO ()
main = print consecutive101Prime