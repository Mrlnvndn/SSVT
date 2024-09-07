import Test.QuickCheck
import Data.List
import Data.Char
import System.Random
import Control.Monad



main :: IO ()
main = do
    let liLength = 10
    liRdmNbr <- replicateM liLength randomNumbGen
    let text = reversalCorectness liRdmNbr
    print text


{-
    Let's start with running through a list comprehension 1000 times and calling the prime function on every value
-}


reversibleStream :: [Int]
reversibleStream = reversedPrimeList where
    li = [ x | x <- [1..10000], prime x] -- Creating a prime List for all Natural numbers until 10000
    revList = [reversal x | x <- li] --Reversing every single prime number and storing them in a list
    reversedPrimeList = [x | x <- revList, prime x] -- Checking if every single value in the previous list is also prime while creating new list


{-
    First case value < 2 so no prime number
    Otherwise we go through another list comprehension and check is the value is divsible by something else than 1 (That's why we start at 2)
    or itself (hence the n-1). We just check for a null modulo result. This then means we get an empty list if it is a prime number
    Null checks if the List is empty, if yes, then returns True  
-}
prime :: Int -> Bool
prime n
    | n < 2 = False
    |otherwise = null [x | x <- [2..n-1], n`mod`x == 0]

--reversible function

reversal :: Int -> Int
reversal  = read . reverse . show 

randomNumbGen :: IO Int
randomNumbGen = randomRIO (0, 10000) 

{-
    reversalCorectness doen't work properly, trying to figure it out
-}

reversalCorectness :: [Int] -> Bool
reversalCorectness li = null [reversal(reversal x) | x <- li, y<- li, x /= y] -- returns true if valid


