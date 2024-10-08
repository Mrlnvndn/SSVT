module Exercise3 where

import Data.List
import Exercise4 (genDomainRelation)
import SetOrd (Set (..), unionSet)
import System.Random
import Test.QuickCheck

type Rel a = [(a, a)]

{-
Exercise 3: Program to get the symmetric closure of relation R.

we use a list comprehension to get the symmetric value of a pair by reorgonising the original tuple. This results
in a list of symmetric relations.
We merge the resulting relations by concatenating them together
Afterwards we get rid of duplicates using 'nub'.
(Since we can't have duplicates in a Set)
-}

symClos :: (Ord a) => Rel a -> Rel a
symClos rel = nub $ concat [[(x, y), (y, x)] | (x, y) <- rel]

{-
Example of a set which is going to become a symmetric closure of it. Also works for randomly generated set.
-}
main :: IO ()
main = do
  let rel = [(1, 2), (2, 3), (3, 4)]
  print $ symClos rel

  (_, randRelation) <- generate (genDomainRelation)
  putStrLn $ "Randomly generated relation:" ++ (show randRelation)
  putStrLn $ "Gives symmetric closure: " ++ (show $ symClos randRelation)