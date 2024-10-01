{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
import Data.List
import Test.QuickCheck
import Mutation (mutators, mutate')
import FitSpec
import Control.Monad 
import Test.QuickCheck.Gen (Gen, vectorOf, choose)
import System.Random


{-
Implement a function that calculates the strength of a given set of properties, which is the
percentage of mutants they kill

Time spent: 70 minutes
-}

{-
Implementing the properties from the FitSpec.hs file, edited to take in 
[Integer] instead of a function. I've done this so I can use these properties
in my countKilled function
-}
-- Property 1: Output list has exactly 10 elements
prop_tenElements' :: [Integer] -> Integer -> Bool
prop_tenElements' l x = length l == 10

-- Property 2: First number is the input
prop_firstElementIsInput' :: [Integer] -> Integer -> Bool
prop_firstElementIsInput' l x = head l == x

-- Property 3: The sum of the output is the input times the 10th triangle number
prop_sumIsTriangleNumberTimesInput' :: [Integer] -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput' l x = sum l == sum [1..10] * x

-- Property 4: The difference between consecutive elements is the input
prop_linear' :: [Integer] -> Integer -> Bool
prop_linear' l x = linear l x

-- Property 5: Any element modulo the input is zero
prop_moduloIsZero' :: [Integer] -> Integer -> Bool
prop_moduloIsZero' l x = x /= 0 --> all (\v -> v `mod` x == 0) l

{-
Reusing function made in Exercise2 to count the number of survivors
but edited to count the number of mutants killed.

countKilled does mainly 3 things:
1. Create a list of mutator functions
2. Check each mutator with each property and a randomized input for the fut
3. Determine which mutants were detected by any of the properties and return the number
-}
countKilled :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Int
countKilled numberOfMutants listOfProperties functionUnderTest = do
  mutations <- getMutants numberOfMutants
  results <- forM mutations $ \mutation ->
    do mutate' mutation listOfProperties functionUnderTest
      <$> generateRandomInteger
  determineSurvivingMutants results

generateRandomInteger :: IO Integer
generateRandomInteger = randomRIO (1, 100)

getMutants :: Integer -> IO [[Integer] -> Gen [Integer]]
getMutants numberOfMutants = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]

determineSurvivingMutants :: [Gen [Bool]] -> IO Int
determineSurvivingMutants propertyPerMutant = do
  generatedPropertyPerMutant <- mapM generate propertyPerMutant
  let livingMutants =  map (not . determineSurvivingMutant) generatedPropertyPerMutant --map determineSurvivingMutant generatedPropertyPerMutant
  return $ length $ filter id livingMutants

determineSurvivingMutant :: [Bool] -> Bool
-- When an empty list is returned, return false as the mutation is equivalent to the original
-- and we do not want this to count as a mutation which is not caught by the properties
determineSurvivingMutant [] = False
determineSurvivingMutant fs = and fs

propStrength :: Integer -> IO Double
propStrength total = do
    let props = [prop_tenElements', prop_firstElementIsInput', prop_sumIsTriangleNumberTimesInput', prop_linear', prop_moduloIsZero']
    -- let props = [prop_tenElements', prop_sumIsTriangleNumberTimesInput', prop_moduloIsZero']
    numKilled <- countKilled total props multiplicationTable
    let totalDouble = fromIntegral total
    return ((fromIntegral numKilled / fromIntegral total) * 100)

main :: IO ()
main = do
    strength <- propStrength 100000
    putStrLn $ "Percentage of mutants killed: " ++ show strength ++ "%"
