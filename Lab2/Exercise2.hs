module Lab2.Exercise2 where

import Control.Monad
import Data.List
import Lab2.FitSpec
import Lab2.Mutation
import System.Random
import Test.FitSpec (results)
import Test.QuickCheck

{-
For exercise 2, we opted to use the mutation functions form Mutation.hs, and the props and fut from FitSpec.hs.
To get this to work, the signature needed to be altered slightly to fit into the mutate function: it now takes
the mutated input as a parameter instead of the fut, so it can check if the property holds on the input and mutated output,
just like the mutate function wants. Furthermore we wrote a function which generates the requested amount of mutants,
based on the list of mutants. Then, every mutation is compared to every property using a randum generated integer as input for the fut
Finally, each mutation evaluation returns an array of bools, indicating if it was killed or not by the properties. This list is then
analyzed and will evaluate to True if all properties returned True, and False (killed) if one of the properties evaluated to False.

+ always catches mutation
~ sometimes catches mutation
- never catches mutation

                | prop_moduloIsZero' | prop_linear' | prop_sumIsTriangleNumberTimesInput' | prop_firstElementIsInput' | prop_tenElements' |
addElements     |         ~          |      ~       |                 ~                   |            ~              |        ~          |
----------------|--------------------|--------------|-------------------------------------|---------------------------|-------------------|
removeElements  |         -          |      -       |                 +                   |            -              |        +          |
----------------|--------------------|--------------|-------------------------------------|---------------------------|-------------------|
anyList---------|         ~          |      ~       |                 ~                   |            ~              |        ~          |

Some side notes on the graph above:
  - anyList generates a list of semi random length with random integers. It is a possibility that it will generate a list which could also be valid output
    from the fut multiplicationTable and be different from the fut output. This would some properties, but the ones which take the function input into account
    would catch the mutant. It could also generate the exact same output as the fut, but this would be caught by mutate' and handled by determineSurvivingMutant.

  - removeElements will never be caught by prop_moduloIsZero', prop_firstElementIsInput', and prop_linear', because at least the first integer
    will remain, and at most only the last number of the original array will be removed which will keep the progression of the integers intact.

  - addElements is tricky, because there is a possibility that the lists added to the start and end of the original array are both empty. This means
    that effectively the list will not change. This would however be caugth by mutate' as it is equal to the original array. For all other properties there
    are posible mutations imaginable which would satisfy it, but never all at the same time.

  - Give the above, with the current combination of properties and generators all mutations will be caught and 0 will be returned every time.

Time spent: 240 minutes
-}

-- countSurvivors does mainly 3 things:
-- 1. Create a list of mutator functions
-- 2. Check each mutator with each property and a randomized input for the fut
-- 3. Determine which mutants were not detected by any of the properties and return the number
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Int
countSurvivors numberOfMutants listOfProperties functionUnderTest = do
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
  let livingMutants = map determineSurvivingMutant generatedPropertyPerMutant
  return $ length $ filter id livingMutants

determineSurvivingMutant :: [Bool] -> Bool
-- When an empty list is returned, return false as the mutation is equivalent to the original
-- and we do not want this to count as a mutation which is not caught by the properties
determineSurvivingMutant [] = False
determineSurvivingMutant fs = and fs

-- Using the slightly altered properties,
-- with the mutated output and original input as parameters

main :: IO ()
main = do
  survivors <- countSurvivors 100000 properties' multiplicationTable
  print survivors