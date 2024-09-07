import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import GHC.Integer (eqInteger)

consecutive101Prime :: Integer
consecutive101Prime = findPrimeSum 0 

findPrimeSum :: Int -> Integer 
findPrimeSum n =
    let res = sumList n in
        if isPrime (doList sumList) then res 
        else findPrimeSum (n+1)

    
{-
findPrimeSum n =
    let sum' = sumList n in
        if isPrime sum' then sum'
        else findPrimeSum (n+1)-}
      
        
sumList :: Int -> Integer
sumList n = toInteger (sum (take 101(drop n doList) ))

doList :: [Integer]
doList = [ toInteger x | x <- [1..], prime x]

prime :: Int -> Bool
prime n
    | n < 2 = False
    |otherwise = null [x | x <- [2..n-1], n`mod`x == 0]

isPrime :: [Integer] -> Integer -> Bool
isPrime li val  =  val `elem` li