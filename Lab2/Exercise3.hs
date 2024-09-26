module Exercise3 where

import Data.Map (Map)
import qualified Data.Map as Map
import Mutation(mutators,mutate')
import Utils
import Props
import Test.QuickCheck
import FitSpec
import Control.Monad
import System.Random

-- Time Spent: -- min (14:42)
{--
Exercise 3: 
Find the definition of the minimal property subset (MPS) in the lecture.
Implement a function that calculates the minimal property subsets,
given a 'function under test' and a set of properties.

Learnings from lecture about MPS:
How do we calculate the minimal property subsets? => 
    We filter the [p]:
        If p kills no mutants → irrelevant
        Kills same mutants as other → equivalent

So esentially we need to identify which p's kill which mutants. Perhaps by creating
a mutation table. Any prop that doesn't kill any mutants is eliminated and for properties
that kill the same properties we need to assess their strength and eliminate the weakest.

# Weaker properties in relation to mutants
A 'weaker' property in this context can be more easily specified as having a high kill-count.
However, we can only remove a weaker property if there are no mutants surviving because of that:
    meaning, mutants not covered in the overlap
This can get quite tricky for large amounts of properties with a lot of overlap, but we keep it
relatively simple for the sake of finishing this exercise.

So our strategy in short:

- Run each property against all mutants and store kill/survived for each combination.
- Calculate 'kill-count' / strength for each property
- Remove kill-less properties
- See if a property envelops another (same mutants + others) -> remove its 'little brother'

--}

{--

--}


generateRandomInteger :: IO Integer
generateRandomInteger = randomRIO (1, 100)

getMutants :: Integer -> IO [[Integer] -> Gen [Integer]]
getMutants numberOfMutants = do
  indices <- replicateM (fromIntegral numberOfMutants) (randomRIO (0, length mutators - 1))
  return [mutators !! i | i <- indices]


-- For reference only
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Int
countSurvivors numberOfMutants listOfProperties functionUnderTest = do
  mutations <- getMutants numberOfMutants
  results <- forM mutations $ \mutation ->
    do mutate' mutation listOfProperties functionUnderTest
      <$> generateRandomInteger
  countSurvivingMutants results

countSurvivingMutants :: [Gen [Bool]] -> IO Int
countSurvivingMutants gens = do
  boolLists <- mapM generate gens
  let genBools = map (\boolList -> return (all (== True) boolList)) boolLists
  trueCounts <- mapM generate genBools
  return $ length $ filter id trueCounts



-- Run one property on each mutation in the list (with random input) and get the results
runProp :: Prop -> [Mutator] -> TypeFut -> IO [Bool]
runProp prop mutations fut = forM mutations $ \mutation -> do 
    randInput <- generateRandomInteger
    resForMutator <- generate (mutate' mutation [prop] fut randInput)
    return (all (== True) resForMutator)

-- Run list of properties against n mutants and create a map with property names
createPropMutantMatrix :: PropMap -> Integer -> TypeFut -> IO (Map String [Bool])
createPropMutantMatrix propMap nMutants fut = do
    mutations <- getMutants nMutants
    propResults <- mapM (\p -> runProp p mutations fut) (Map.elems propMap)
    return $ Map.fromList (zip (Map.keys propMap) propResults)


-- Calculates Minimal Subset of Properties of a combination of a Function Under Test
-- and a set of properties
minPropSubset :: TypeFut -> [Prop] -> [Prop]
minPropSubset fut props = props


main :: IO ()
main = do
    -- mutations <- getMutants 3
    -- results <- runProp (properties' !! 1) mutations multiplicationTable
    -- print results
    matrix <- createPropMutantMatrix propMap 5 multiplicationTable
    print matrix