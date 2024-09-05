{-# OPTIONS_GHC -Wno-forall-identifier #-}

module Lab0 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- reversibleStream :: [Integer]
-- reversibleStream = undefined

reversal :: Integer -> Integer
reversal = read . reverse . show

-- rot13 :: [Char] -> [Char]
-- rot13 = undefined

-- consecutive101Prime :: Integer
-- consecutive101Prime = undefined

-- euler9 :: Integer
-- euler9 = undefined

-- euler10 :: Integer
-- euler10 = undefined

-- euler49 :: Integer
-- euler49 = undefined