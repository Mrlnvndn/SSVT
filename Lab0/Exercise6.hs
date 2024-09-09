module Exercise6 where

import Data.Foldable
import Data.List
import Lab0

-- Time spent: 60 min
{-
Exercise: Find the smallest prime number that is a sum of 101 consecutive primes.

We have set out to implement this using a list of endless primes.
That is not necessary as I could also just get the first 40000 primes, which I now know contains the answer.
However, this is a nice challenge, as working with inifinite lists makes calculation a lot harder conceptually.

Ultimately, we did not have time to make a test property, but in theory you could easily test whether:
- the resulting list contains 101 values
- ,which are actually prime

Whether the result is actually the smallest possible is much harder to prove unless there is some way to do it using induction
(or verify that every smaller combination does not work).

-}

-- Implementation of Sieve of Eratosthenes to calculate primes using:
--  https://stackoverflow.com/questions/56119201/primes-in-haskell
sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve (filter (\x -> mod x p > 0) xs)
allPrimes :: [Integer]
allPrimes = sieve [2..]


-- Stack overflow inspired function: https://stackoverflow.com/questions/51581983/is-there-a-way-to-pattern-match-for-length-in-haskell
-- Amazing function that gives all sublists of length n, which also works on infinite lists!
neighbors :: Int -> [a] -> [[a]]
neighbors n l = zipWith (const . take n) (tails l) (drop (n-1) l)

neighborSum = [sum n | n <- neighbors 101 allPrimes]

consecutive101Prime :: Integer
consecutive101Prime = last $ takeWhile (not . prime) neighborSum

main = do 
    consecutive101Prime
