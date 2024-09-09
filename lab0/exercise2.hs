import Test.QuickCheck
import Data.List
import Data.Char
import System.Random

{- 
    Time spent: 120 Minutes
-}

main :: IO()
main = do -- <- unwraps a monad and return wraps a monad again
    let nbr = 10000
    -- getting the list of random values and unwrapping it
    li <- probs nbr
    -- calling my sorting function for each quartile
    let firstQuart = createQuartile li 0.0 0.25
    let scdQuart = createQuartile li 0.25 0.50
    let thrdQuart = createQuartile li 0.50 0.75
    let fourthQuart = createQuartile li 0.75 1.00
    --to represent a 99% confidence level
    let lowerConfLev = 2400
    let upperConfLevel = 2600
    
    let checkedFirst = checkQuartile lowerConfLev upperConfLevel firstQuart
    let checkedscd = checkQuartile lowerConfLev upperConfLevel scdQuart
    let checkedthrd = checkQuartile lowerConfLev upperConfLevel thrdQuart
    let checkedfourth = checkQuartile lowerConfLev upperConfLevel fourthQuart


{-
    Based on multiple tests of my functions, assuming we are using 10.000 values to test, a 99% precision can be used to fulfill 
    our needs. reason being -> never less than 2400 and never more than 2600 values will be summed for a quartile.
-}

    -- here we could implement a function to check if the sum of the amount of values in each quartile is equal to the total given in the beginning
    print checkedFirst
    print checkedscd
    print checkedthrd
    print checkedfourth
    
    




probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)


{-
    This function uses list comprehension with upper and lower values to
     categorize every single value in the received list
-}

createQuartile :: [Float] -> Float -> Float -> Int
createQuartile li lower upper = length[x| x<-li, x<upper && x>=lower]

checkQuartile :: Int -> Int -> Int -> Bool
checkQuartile lower upper val = val < upper && val > lower


    