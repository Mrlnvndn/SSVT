{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
import Control.Monad
import Data.List
import FitSpec
import Mutation
import System.Random
import Test.FitSpec (results)
import Test.QuickCheck

-- Time spent: 120 Mins

main :: IO ()
main = do
  -- Loop through each pair of properties
  forM_ [0 .. (length properties' - 1)] $ \i -> do
    forM_ [0 .. (length properties' - 1)] $ \j -> do
      when (i /= j) $ do
        let prop1 = properties' !! i
        let prop2 = properties' !! j
        putStrLn ""
        putStrLn $ "Comparing Property " ++ show i ++ " with Property " ++ show j
        -- Generates 1000 Mutants where every property is going to be checked on
        res1 <- equalProps' 1000 [prop1, prop2] >>= mapM generate
        let equalCheck = eqTuples res1
        if equalCheck
          then do
            putStrLn "Properties are equal"
          else do
            -- Checking if property j is a subset of property i
            let subsCheck = checkForSubs $ doTuples res1
            -- If it is not then inverse the placement of the results in the tuples to check whether property i is a subset of property j
            if not subsCheck
              then do
                let invertedTuples = map (\(a, b) -> (b, a)) (doTuples res1)
                let subsCheckInverted = checkForSubs invertedTuples
                if not subsCheckInverted
                  then do
                    putStrLn $ "Property " ++ show i ++ " and Property " ++ show j ++ " are not equal, and none of them is a subset of the other"
                  else putStrLn $ "Property " ++ show i ++ " is a subset of Property " ++ show j
              else putStrLn $ "Property " ++ show j ++ " is a subset of Property " ++ show i
  putStrLn "Comparison complete"

{-
    To check the equivalence of 2 properties we want to pass them in the mutate function with the same mutations
    a certain amount of times (different mutations every iteration) and compare the amount of returned falses
    in the list (indicating that the mutant has been killed). If the length of the list equivalent then the
    properties are equivalent.
-}

-- Gets a value that determines the amount of times 2 properties will be mutated and checked whether they are getting killed or not. This will return
-- a list of lists with boolean values, false if killed and true if survived.
equalProps' :: Integer -> [[Integer] -> Integer -> Bool] -> IO [Gen [Bool]]
equalProps' nb props = do
  mutator <- randomMutator
  return $ replicate (fromIntegral nb) (mutate' mutator props multiplicationTable 10)

-- Function to randomly select a mutator from the list
randomMutator :: IO ([Integer] -> Gen [Integer])
randomMutator = do
  index <- randomRIO (0, length mutators - 1)
  return (mutators !! index)

-- Takes a list of lists, transforms it into a list of tuples and then compares all the first elements to the second elements to verify if
-- two properties are equal. Returns true if they are.
eqTuples :: (Eq a) => [[a]] -> Bool
eqTuples xs = all (uncurry (==)) $ doTuples xs

doTuples :: [[a]] -> [(a, a)]
doTuples li = [(x, y) | [x, y] <- li]

{-
    A subset of a property is a property that kills less mutants than the property it is a subset of, but the ones being killed by it, should all also
    be killed by the greater property.

    This function will check if the property resulting in the second element of the tuple is a subproperty of the property resulting in the first element of the tuple. In essence we have to check that every second
    element of the tuple returns either True or False when the first element is False. It cannot return False when the first element return true.
    The two properties being equal is already ruled out by our eqTuples function that got applied before this one. If every tuples in the list
    is equal then the properties are equal and this checkForSubs function won't even be called.
-}
checkForSubs :: [(Bool, Bool)] -> Bool
checkForSubs = all (\(a, b) -> not (a && not b))

-- Choose 2 properties that are going to get tested for equivalence
-- properties' = [prop_moduloIsZero', prop_moduloIsZero', prop_firstElementIsInput', prop_sumIsTriangleNumberTimesInput', prop_tenElements']
