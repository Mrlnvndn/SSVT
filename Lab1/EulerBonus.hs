module EulerBonus where
import Data.Foldable

{--
Euler problem 9: Find pythogarean triplet where a + b + c = 1000

This one was easy to brute force. I basically just used code from Lab0 and tweaked it to check for the
condition stated in problem 9. Going through all possible combinations is quite fast, so I did not optimise my solution much.
--}


-- naive approach, using the fact that a < b < c to limit ranges
-- Taken from my test generation for Lab0 Ex 3 with extra check for a+b+c=1000
genPythagoreanTriples :: Integer -> [(Integer, Integer, Integer)]
genPythagoreanTriples n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2, a+b+c == 1000]

-- Find triples up to c=500 and take the product of the triple:
-- Answer comes out to 31875000
euler9 :: Integer
euler9 = do
    let (a, b, c) = head (genPythagoreanTriples 500)
    a * b * c


{--
Euler problem 10: Find the sum of all the primes below two million

I actually tried a bunch of methods to solve this problem. The main issue was of course performance and
memory usage. Since it is quite difficult to assess how much memory and recursion is occuring in a Haskell program
it took quite a while to figure out the best approach. From Lab0 I already learned that the 'sieve of eratosthenes'
is a useful algorithm if you want to get all primes up to a certain value. It eliminates a lot of values, meaning not every
value has to be cheched.

The biggest improvement was not trying to generate the entire list first (as I was doing with my 'sieve' function in Lab0)
but instead immediately summing the outcomes. Other important efficiency upgrades were:
- only using odd numbers
- jumping by squares of last prime (anything not elinated in the skipped range cannot be eliminted by found primes anyway)
(I tried using the fact that all primes after 2,3 satisfy 6n-1=p or 6n+1 = p (either value below or above is divisable by 6))
This is function primeCandidates. However, this broke my additiveSieve, because ps could now be empty somehow)

Answer comes out to: 142913828922, which takes around 15 seconds on my machine
--}

-- Implementation of Sieve of Eratosthenes to calculate primes using:
--  https://stackoverflow.com/questions/56119201/primes-in-haskell
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve (filter (\x -> mod x p > 0) xs)

isNat :: (Real a, RealFrac a) => a -> Bool
isNat x = fromInteger (floor x) == x

primeCandidates :: Integer -> [Integer]
primeCandidates n = [x | x <- [3,5..n], isNat (fromInteger (x+1)/6.0 :: Double) || isNat (fromInteger (x-1)/6.0 :: Double)]


additiveSieve :: Integral a => [a] -> a -> a
additiveSieve [] _ = 0
additiveSieve l lastPrime = do
    let (ps, ys) = span (< lastPrime^2) l
    let lastPrime' = last ps
    sum ps + additiveSieve ([z | z <- ys, and [z `mod` y > 0 | y <- ps]]) lastPrime'


euler10 :: Integer
euler10 = 2 + additiveSieve [3,5..2_000_000] 2
        
