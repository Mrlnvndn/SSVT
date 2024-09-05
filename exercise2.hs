import Test.QuickCheck
import Data.List
import Data.Char
import System.Random

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
    let confLevel = 0.01 --to represent a 99% confidence level
    -- here we could check if the sum of the amount of values in each quartile is equal to the total given in the beginning
    print firstQuart
    print scdQuart
    print thrdQuart
    print fourthQuart
    




probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)


{-
    First of all we are going to have a function that sorts our values in the 4 quartiles

-}

createQuartile :: [Float] -> Float -> Float -> Int
createQuartile li lower upper = length[x| x<-li, x<upper && x>=lower]


    