import Data.List
import Test.QuickCheck
import Mutation (mutators)
import FitSpec

-- properties 
properties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

-- Generates the mutantors 
genMutators :: [[Integer] -> Gen [Integer]]
genMutators =
    [anyList, removeElements, addElements]

-- Add argument to add genMutants 
countSurvivors :: Integer -> [(Integer -> [Integer]) -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> Int
countSurvivors nMutants listProp f mutators = do
    -- Generate mutations
    -- length [k | k <- [0..nMutants], not (mutate' (mutators !! 0) listProp f 10)]
    -- results <- mapM (mutateAndCheck listProp f) [0..nMutants-1]
    -- return $ length $ filter id results
    -- filterM (mutate' (mutators !! 0) listProp f 10) [0..nMutants]
    survivors <- filterM checkMutant [0..nMutants-1]
    return $ length survivors

checkMutant :: [Integer] -> [(Integer -> [Integer]) -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Bool
checkMutant mutantIndex properties funct = do
        survfilterM checkMutant [0..nMutants-1]
        -- Pick a mutator for the mutant (based on index)
        let mutator = genMutators !! (fromIntegral (mutantIndex `mod` length mutators))
        -- Generate the mutated output
        mutated <- generate (mutator (funct mutantIndex))
        -- Check all properties; if any property fails, the mutant is killed
        return $ all (\prop -> prop mutantIndex mutated) properties

-- Generate mutations
-- Pass properties through the mutants
-- Get the numebr that survive

-- main = 
--     let properties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]
--     in countSurvivors 1 properties multiplicationTable








