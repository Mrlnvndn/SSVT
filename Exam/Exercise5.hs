module Exam.Exercise5 where

import Control.Monad (forM, replicateM)
import Data.List (subsequences)
import System.Random (randomRIO)
import Test.QuickCheck (Gen, choose, generate)
import Test.QuickCheck.Gen (Gen, choose, vectorOf)

-- == Problem 5 ==
corona :: Fut
corona r s x0 t = r ^ t * x0 + s * r ^ t / r

-- Properties
prop_increaseAsTimeGoesUp :: Prop
prop_increaseAsTimeGoesUp output r s x0 t = output < corona r s x0 (t + 1)

prop_FollowsRecursiveDef :: Prop
prop_FollowsRecursiveDef output r s x0 t = corona r s x0 (t + 1) - output == (r - 1) * output + s

-- Mutators
mutators :: [Double -> Gen Double]
mutators = [coronaMutator1, coronaMutator2]

coronaMutator1 :: Double -> Gen Double
coronaMutator1 output = do
  rand <- choose (1, 2)
  return (output + rand)

coronaMutator2 :: Double -> Gen Double
coronaMutator2 output = do
  rand2 <- choose (1, 1.1)
  return (output * rand2)

-- Change the following types according to the  input and output
type Input = (Double, Double, Double, Int)

type Output = Double

-- change the input
type Fut = Double -> Double -> Double -> Int -> Output

-- change the input
type Prop = Output -> Double -> Double -> Double -> Int -> Bool

type Mut = Output -> Gen Output

-- fut input: (num a, eq a) =>  a -> a -> a -> Int, returns a
-- property input: (num a, eq a) =>  a -> a -> a -> Int, returns Bool
-- mutator input: (num a, eq a) => a, returns a

-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (False), whether it lives (True), or that the mutant did not change the output (empty list)
mutate' :: Mut -> [Prop] -> Fut -> Input -> Gen [Bool]
mutate' mutator props fut (input1, input2, input3, input4) = mutation >>= \mutant -> mutateOrNothing' output mutant (propertyExecutor' props mutant (input1, input2, input3, input4))
  where
    output = fut input1 input2 input3 input4
    mutation = mutator output

propertyExecutor' :: [Prop] -> Output -> Input -> Gen [Bool]
propertyExecutor' props mutant (input1, input2, input3, input4) = return $ map (\y -> y mutant input1 input2 input3 input4) props

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing' :: Output -> Output -> Gen [Bool] -> Gen [Bool]
mutateOrNothing' output mutation res
  | output == mutation = return []
  | otherwise = res

countKilled :: Integer -> [Prop] -> Fut -> IO Int
countKilled numberOfMutants listOfProperties functionUnderTest = do
  mutations <- getMutants numberOfMutants
  results <- forM mutations $ \mutation ->
    do mutate' mutation listOfProperties functionUnderTest
      <$> generateRandomInput
  determineSurvivingMutants results

-- Adapt this to the required input
generateRandomInput :: IO Input
generateRandomInput = do
  a <- randomRIO (1, 100)
  b <- randomRIO (1, 100)
  c <- randomRIO (1, 100)
  d <- randomRIO (1, 100)
  return (a, b, c, d)

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

getMutants :: Integer -> IO [Output -> Gen Output]
getMutants numberOfMutants = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]

propStrength :: Integer -> [Prop] -> IO Double
propStrength total props = do
  numKilled <- countKilled total props corona
  let totalDouble = fromIntegral total
  return ((fromIntegral numKilled / fromIntegral total) * 100)

showStrengths :: [Prop] -> [[(Prop, String)]] -> Integer -> IO ()
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

propsWithNames :: [(Prop, String)]
propsWithNames =
  [ (prop_increaseAsTimeGoesUp, "prop_increaseAsTimeGoesUp'"),
    (prop_FollowsRecursiveDef, "prop_FollowsRecursiveDef'")
  ]

getSubSets :: [(Prop, String)] -> [[(Prop, String)]]
getSubSets propsWithNames = filter (not . null) (subsequences propsWithNames)

main :: IO ()
main = do
  let nMutants = 100000
  let props = map fst propsWithNames
  let propSubsets = getSubSets propsWithNames -- Generate all non-empty subsets
  showStrengths props propSubsets nMutants