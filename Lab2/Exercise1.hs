module Exercise1 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable (multiplicationTable)
import System.Random
import Utils ( fisherYates )


-- Time Spent: -- min

{-- Lab2
# Exercise 1: Create a list of mutators

In Mutation.hs are already a few examples of possible mutations
on an output list. We are tasked to come up with other ways of covering
possible outputs and implementing some of those.

This is a tricky mainly because we know nothing about what kind of function
we are testing. This means we don't have any strong expectations about how the output
should look. However, looking at generic mutators for testing any function that outputs
a list, stronger mutators are operations that target very strong (specific) properties.

This means we should try to make minimal changes to the output that are still viable mutators.


## Analysis of currently provided mutators

* Mutation.addElements: A strength of this mutator is that it potentially tests
    two properties at the same time (sensitivity to contents at start and end of the list).
    It weakness is that it changes the length of the list, which is likely caught by multiple properties
* Mutation.removeElements: (similar strengths / weaknesses to previous)
* Mutation.anyList: This is the weakest mutation we can do.
    This just changes the output for anything else. These mutations will very rarely survive
    because the chance that a pattern or property is completely randomly generated is quite low.

## So what is missing?
There are a lot of operations on integers (either one, a section or the entire list) that are now not
covered. Also there are some more specific changes you could make to a list (like changing only one entry)
,which might also be a stronger mutator than the ones provided.

## Possible options for mutators:
Importantly: we are testing a list of Integers, meaning we need to mutate the list, its contents
or both.

1. Change the order of an output list (we could sort or randomly change indices of value(s))
2. Replace one or multiple values with a random different value (stronger version of anyList)
3. Test the impact of changing integer sign (make postive values negative)
4. Add element to the middle (or any part) of the list
5. Mapping some function on the integers with a pattern (squaring everything, sign function)
6. Output an empty list (quite weak, but edge case that might not be specifically tested for)

--}

-- Helper functions:




-- Mutators:

reorderSort :: [Integer] -> [Integer]
reorderSort = sort

reorderShuffle :: [Integer] -> IO [Integer]
reorderShuffle xs = do
  randomGen <- newStdGen
  return $ fst $ fisherYates randomGen xs

addMiddleElement :: [Integer] -> Gen [Integer]
addMiddleElement xs = do
  let (ys, zs) = splitHalf xs
  newValue <- arbitrary
  return $ ys ++ [newValue] ++ zs

splitHalf :: [a] -> ([a], [a])
splitHalf l = Data.List.splitAt ((length l + 1) `div` 2) l


emptyList :: [Integer] -> [Integer]
emptyList _ = []


main = do
    testList <- generate $ listOf arbitrary
    putStrLn $ "Randomly generated arbitrary list: " ++ show testList

    putStrLn $ "Sorted: " ++ show (reorderSort testList)

    shuffleRes <- reorderShuffle testList
    putStrLn $ "Random shuffle: " ++ show (shuffleRes)

    addedMiddle <- generate $ addMiddleElement testList
    putStrLn $ "Added middle element: " ++ show addedMiddle

    putStrLn $ "Empty list mutation: " ++ show (emptyList testList)





