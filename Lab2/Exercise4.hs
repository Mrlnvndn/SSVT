{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}

import Control.Monad
import Data.List
import Lab2.FitSpec
import Lab2.Mutation (mutate', mutators)
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen, choose, vectorOf)

{-
Implement a function that calculates the strength of a given set of properties, which is the
percentage of mutants they kill

Time spent: 70 minutes
-}

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
  let livingMutants = map (not . determineSurvivingMutant) generatedPropertyPerMutant -- map determineSurvivingMutant generatedPropertyPerMutant
  return $ length $ filter id livingMutants

determineSurvivingMutant :: [Bool] -> Bool
-- When an empty list is returned, return false as the mutation is equivalent to the original
-- and we do not want this to count as a mutation which is not caught by the properties
determineSurvivingMutant [] = False
determineSurvivingMutant fs = and fs

-- Function to calculate propStrength for a list of properties
propStrength :: Integer -> [[Integer] -> Integer -> Bool] -> IO Double
propStrength total props = do
  numKilled <- countKilled total props multiplicationTable
  let totalDouble = fromIntegral total
  return ((fromIntegral numKilled / fromIntegral total) * 100)

-- Two mehtods for a hacky way to be able to print property names
printFunctionNames :: [([Integer] -> Integer -> Bool, String)] -> IO ()
printFunctionNames function = mapM_ (putStrLn . snd) function

propsWithNames :: [([Integer] -> Integer -> Bool, String)]
propsWithNames =
  [ (prop_tenElements', "prop_tenElements'"),
    (prop_firstElementIsInput', "prop_firstElementIsInput'"),
    (prop_sumIsTriangleNumberTimesInput', "prop_sumIsTriangleNumberTimesInput'"),
    (prop_linear', "prop_linear'"),
    (prop_moduloIsZero', "prop_moduloIsZero'")
  ]

showStrengths :: [[Integer] -> Integer -> Bool] -> [[([Integer] -> Integer -> Bool, String)]] -> Integer -> IO ()
showStrengths props subsets nMutants = do
  strengths <- mapM (propStrength nMutants . map fst) subsets
  mapM_
    ( \(subset, strength) ->
        putStrLn $
          "Subset: "
            ++ show (map snd subset)
            ++ " -> Percentage of mutants killed: "
            ++ show strength
            ++ "%"
    )
    (zip subsets strengths)

getSubSets :: [([Integer] -> Integer -> Bool, String)] -> [[([Integer] -> Integer -> Bool, String)]]
getSubSets propsWithNames = filter (not . null) (subsequences propsWithNames)

main :: IO ()
main = do
  let nMutants = 100000
  let props = map fst propsWithNames
  let propSubsets = getSubSets propsWithNames -- Generate all non-empty subsets
  showStrengths props propSubsets nMutants