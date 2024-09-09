import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import GHC.Event.Windows (ioFailed)
import Control.Applicative (Alternative(some))
{-
The code given includes an IO because it uses the getStdRandom. 
If the IO wasn't used it would result in the same random number 
everytime, which is undesirable. 

research for why 10000 and ranges are why they are 
Time spent: 200 minutes
-}

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

creatSublist :: (Float, Float) -> [Float] -> [Float]
creatSublist (lower, upper) f  =
    [k | k <- f, k >= lower && k < upper]

quarter :: Int -> IO [[Float]]
quarter n = do
    resultProbs <- probs n
    let subList1 = creatSublist (0, 0.25) resultProbs
    let subList2 = creatSublist (0.25, 0.50) resultProbs
    let subList3 = creatSublist (0.50, 0.75) resultProbs
    let subList4 = creatSublist (0.75, 1) resultProbs
    return [subList1, subList2, subList3, subList4]

getAmountInQuarters :: Float -> IO [[Float]] -> IO [Float]
getAmountInQuarters n quarter = do
    quarterList <- quarter
    let quartile1 = take 1 quarterList 
    let quartile2 = take 2 quarterList 
    let quartile3 = take 3 quarterList 
    let quartile4 = take 4 quarterList
    return [fromIntegral (length (concat quartile1)) / n, fromIntegral (length (concat quartile2)) / n, fromIntegral  (length (concat quartile3)) / n, fromIntegral (length (concat quartile4)) / n] 



main = do -- declares youre gonna work in an IO
    putStrLn "Testing probs"
    -- result <- probs 10000 -- actually unwraps it and now you dont have to worry about IO
    -- let resultProb = creatSublist (0, 0.25) result
    -- let len = length resultProb 
    -- print len

    result <- getAmountInQuarters 10 (quarter 10)
    print result
    -- let first = take 2 result
    -- print first


