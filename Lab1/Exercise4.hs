import Data.List
import Lecture2
import System.Random
import Test.QuickCheck

{-
1. Implementation of isDerangement which checks if a list (xs) is a derangement of another list (ys)
by checking if the lists abide to three properties.
-}
isDerangement :: (Eq a) => [a] -> [a] -> Bool
isDerangement xs ys = sameLength xs ys && allDifferent xs ys && sameElements xs ys

-- Checks if the lists have the same length
sameLength :: [a] -> [a] -> Bool
sameLength xs ys = length xs == length ys

-- Checks if all items in the list are in a different place
allDifferent :: (Eq a) => [a] -> [a] -> Bool
allDifferent xs ys = all (uncurry (/=)) (zip xs ys)

-- Checks if the lists contain the same items
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

{-
2. Implementation of deran which generates all derangements using the permutations function and the allDifferent function defined above
to make sure ony permutations which are also derangements are returend. Because of the use of perumtations, sameElements and sameLength are true by default
so they do not need to be checked
-}
deran :: Int -> [[Int]]
deran 0 = [[]]
deran n = [perm | perm <- permutations [1 .. n], allDifferent [1 .. n] perm]

{-
3. Testable properties based on functions above used to check if one set is a derangement of another one
-}
prop_areSameLength :: Int -> Bool
prop_areSameLength size = all (\perm -> length perm == size) (deran size)

prop_areAllDifferent :: Int -> Bool
prop_areAllDifferent size = all (\perm -> allDifferent perm [1 .. size]) (deran size)

prop_haveSameElements :: Int -> Bool
prop_haveSameElements size = all (\perm -> sameElements perm [1 .. size]) (deran size)

-- Property definitions (with a name to allow for easy printing)
type Prop = (String, PropFn)

type PropFn = Int -> Bool

p1, p2, p3, p4 :: PropFn
p1 n = even n && n > 3 -- From first equation: (\ x -> even x && x > 3)
p2 n = even n || n > 3 -- From second equation: (\ x -> even x || x > 3)
p3 n = (even n && n > 3) || even n -- From third equation: (\ x -> (even x && x > 3) || even x)
p4 = p3 -- Right side of fourth equation is the same as p3

{-
4. & 5. Similar code to Exercise3.hs to automatically determine the relative weakness of the properties

The answer our implementation spits out is (order from strongest to weakest):
["prop_haveSameElements","prop_areSameLength","prop_areAllDifferent"]
-}
propList :: [Prop]
propList =
  [ ("prop_areAllDifferent", prop_areAllDifferent),
    ("prop_areSameLength", prop_areSameLength),
    ("prop_haveSameElements", prop_haveSameElements)
  ]

-- Same method used to order the properties as explained in Exercise3.hs
quicksort' :: [Int] -> [Prop] -> [Prop]
quicksort' _ [] = []
quicksort' l (x : xs) =
  quicksort' l [a | a <- xs, stronger l (snd a) (snd x)]
    ++ [x]
    ++ quicksort' l [a | a <- xs, not $ stronger l (snd a) (snd x)]

testDescending :: [Prop] -> Bool
testDescending [] = True
testDescending [_] = True
testDescending (x : y : rest) =
  stronger [1 .. 10] (snd x) (snd y)
    && testDescending rest

{-
Range between 0 and 9 is chosen for each property to make the compute time reasonable,
because increasing the length by one increases the number of derangements by roughly tenfold.
So this way we keep the time and space complexity low, while also showing it works for lists upto length 10
-}
main = do
  let domain = [0 .. 9] -- Small range of integers as suggested in ex.
  let sortedProps = quicksort' domain propList
  putStrLn "Order from strongest to weakest:"
  print [fst s | s <- sortedProps] -- show list with names
  if testDescending sortedProps
    then putStrLn "Passed descending strength test!"
    else putStrLn "Descending strength test: Failed!"