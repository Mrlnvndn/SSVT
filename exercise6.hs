import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import GHC.Integer (eqInteger)
--Time spent: 120 Minutes

{-
    consecutive101Prime is our initial function
-}
consecutive101Prime :: Integer
consecutive101Prime = findPrimeSum 0 

--loops through the whole process incrementing the n to drop a certain amount of values from the head of the prime list
findPrimeSum :: Int -> Integer 
findPrimeSum n =
    let li = doList in
    let sum' = sumList n li in
        if isPrime(li sum')  then sum'
        else findPrimeSum (n+1)

--sums up a consecutive of 101 prime numbers from the prime list by dropping the head of the prime list if end result is not prime
              
sumList :: Int -> [Integer] -> Integer
sumList n  li = toInteger (sum (take 101(drop n li) ))

-- computes a list of primes
doList :: [Integer]
doList = [ toInteger x | x <- [1..], prime x]

--Checks if value is prime to compose prime list
prime :: Int -> Bool
prime n
    | n < 2 = False
    |otherwise = null [x | x <- [2..n-1], n`mod`x == 0]

--Checks wether value is in the prime list
isPrime :: [Integer] -> Integer -> Bool
isPrime li val  =  val `elem` li

-- we could test the code by checking wether the result is Prime
checkIfPrime:: Bool
checkIfPrime = isPrime doList consecutive101Prime 