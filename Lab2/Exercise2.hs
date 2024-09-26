module Exercise2 where

import Control.Monad
import Data.List
import FitSpec
import Mutation
import System.Random
import Test.FitSpec (results)
import Test.QuickCheck
import Text.Read.Lex qualified as FitSpec

{-
For exercise 2, we opted to use the mutation functions form Mutation.hs, and the props and fut from FitSpec.hs\
To get this to work, the signature needed to be altered slightly to fit into the mutate function: it now takes
the mutated input as a parameter instead of the fut, so it can check if the property holds on the input and mutated output,
just like the mutate function wants. Furthermore we wrote a function which generates the requested amount of mutants,
based on the list of mutants.

-}

countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Int
countSurvivors numberOfMutants listOfProperties functionUnderTest = do
  mutations <- getMutants numberOfMutants
  results <- forM mutations $ \mutation ->
    do mutate' mutation listOfProperties functionUnderTest
      <$> generateRandomInteger
  countSurvivingMutants results

generateRandomInteger :: IO Integer
generateRandomInteger = randomRIO (1, 100)

getMutants :: Integer -> IO [[Integer] -> Gen [Integer]]
getMutants numberOfMutants = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]

countSurvivingMutants :: [Gen [Bool]] -> IO Int
countSurvivingMutants gens = do
  boolLists <- mapM generate gens
  let genBools = map (\boolList -> return (all (== True) boolList)) boolLists
  trueCounts <- mapM generate genBools
  return $ length $ filter id trueCounts

countJustTrues :: [Gen (Maybe Bool)] -> IO Int
countJustTrues gens = do
  values <- mapM generate gens
  return $ length $ filter (== Just True) values

properties' = [prop_moduloIsZero', prop_linear', prop_sumIsTriangleNumberTimesInput', prop_firstElementIsInput', prop_tenElements']

prop_moduloIsZero' :: [Integer] -> Integer -> Bool
prop_moduloIsZero' mutation input = input /= 0 --> all (\v -> v `mod` input == 0) mutation

prop_linear' :: [Integer] -> Integer -> Bool
prop_linear' = linear

prop_sumIsTriangleNumberTimesInput' :: [Integer] -> Integer -> Bool
prop_sumIsTriangleNumberTimesInput' mutation input = sum mutation == sum [1 .. 10] * input

prop_firstElementIsInput' :: [Integer] -> Integer -> Bool
prop_firstElementIsInput' [] _ = False
prop_firstElementIsInput' mutation input = head mutation == input

prop_tenElements' :: [Integer] -> Integer -> Bool
prop_tenElements' mutation input = length mutation == 10