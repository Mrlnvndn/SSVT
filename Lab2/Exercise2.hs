{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
import Data.List
import Test.QuickCheck
import Mutation (mutators, mutate')
import FitSpec
import Control.Monad (filterM)

-- properties 
-- properties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

-- Generates the mutantors 
genMutators :: [[Integer] -> Gen [Integer]]
genMutators =
    -- [anyList, removeElements, addElements]
    mutators

-- Properties 
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

-- Add argument to add genMutants 
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Int
countSurvivors nMutants listProp f mutators = do
    -- Generate mutations
    -- len6gth [k | k <- [0..nMutants], not (mutate' (mutators !! 0) listProp f 10)]
    -- results <- mapM (mutateAndCheck listProp f) [0..nMutants-1]
    -- return $ length $ filter id results
    -- filterM (mutate' (mutators !! 0) listProp f 10) [0..nMutants]
    -- let survivors = mapM checkMutant [0..nMutants-1]
    results <- mapM checkMutant [0..nMutants-1] 
    let survivors = filter (all id) results
    -- (length (concat survivors))
    return $ length survivors
    where
        checkMutant :: Integer -> IO [Bool]
        checkMutant input = do
                -- Rolls through the mutators
                let mutator = genMutators !! fromIntegral (input `mod` fromIntegral (length genMutators))
                -- Apply mutator to properties
                -- let propertiesResult = mutate' mutator listProp f input
                propertiesResult <- generate $ mutate' mutator listProp f input
                -- Check all properties; if any property fails, the mutant is killed
                return propertiesResult -- this will return a bunch of subsets 2
                -- where
                --     checkProperty :: ([Integer] -> Gen [Integer]) -> Gen [Bool]
                --     checkProperty mutator = do
                --         -- apply mutate function to each property
                --         -- mapM (\props -> mutate' mutator props f input) listProp 
                --         mutate' mutator listProp f input



-- Generate mutations
-- Pass properties through the mutants
-- Get the numebr that survive

main :: IO ()
main = do
    let props = [prop_tenElements', prop_firstElementIsInput', prop_sumIsTriangleNumberTimesInput', prop_linear', prop_moduloIsZero']
    survivors <- countSurvivors 4000 props multiplicationTable mutators
    putStrLn $ "Number of survivors: " ++ show survivors









