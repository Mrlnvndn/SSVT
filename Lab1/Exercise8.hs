import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lecture2
import SetOrd

{-
Time Spent: 120 Minutes
    To refute the conjecture we have to come up with a list of tuples where the second element is not a prime number
    Finding ANY value in an infinite list of primes where their product+1 isn't prime will automatically refute the conjecture

-}
-- Creating a list of tuples with the list of prime numbers as the first element of the tuple, and the product of those primes +1 as the second element
-- The list comprehension filters the outputs, by just returning the prod elements that are not prime and in this case
-- refuting the conjecture
counterexamples :: [([Integer], Integer)]
counterexamples = nub [ (listPrime, prod) | n <- [1..], let listPrime = createPrimeList n, let prod = product listPrime + 1, not(isPrime prod) ]
--nub gets rid of duplicates in the list which are unwanted


--Creates a list of primes using the Isprime property
createPrimeList :: Integer -> [Integer]
createPrimeList n = [x | x <- [0..n], isPrime x]

--Function checking if a number is prime
isPrime :: Integer -> Bool
isPrime n 
    | n < 2 = False
    |otherwise = null [x | x <- [2..n-1], n`mod`x == 0]
