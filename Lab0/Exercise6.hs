module Exercise6 where

import Data.Foldable
import Data.List
import Lab0

-- Time spent: 60 min
-- so far...
{-
Exercise: Find the smallest prime number that is a sum of 101 consecutive primes.

I have set out to implement this using a list of endless primes.
That is not necessary as I could also just get the first 40000 primes, which I now know contains the answer.
However, this is a nice challenge, as working with inifinite lists makes calculation a lot harder conceptually.

THE OUTCOME STILL HAS TO BE PROVEN or tested in some way!!!

-}

-- Implementation of Sieve of Eratosthenes to calculate primes using:
--  https://stackoverflow.com/questions/56119201/primes-in-haskell
sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve (filter (\x -> mod x p > 0) xs)
allPrimes :: [Integer]
allPrimes = sieve [2..]

takeLeftover = const . drop 1

-- lastN :: Int -> [a] -> [a]
-- lastN n xs = foldl' takeLeftover xs (drop n xs)


-- Stack overflow inspired function: https://stackoverflow.com/questions/51581983/is-there-a-way-to-pattern-match-for-length-in-haskell
-- Amazing function that gives all sublists of length n, which also works on infinite lists!
neighbors :: Int -> [a] -> [[a]]
neighbors n l = zipWith (const . take n) (tails l) (drop (n-1) l)


-- sum101Primes :: [Integer] -> Integer
-- sum101Primes [] = 0
-- sum101Primes (p:xs) = p : sum101Primes

neighborSum = [sum n | n <- neighbors 101 allPrimes]

consecutive101Prime :: Integer
consecutive101Prime = last $ takeWhile (not . prime) neighborSum

main = do 
    consecutive101Prime
    -- take 10 (neighbors 5 allPrimes)