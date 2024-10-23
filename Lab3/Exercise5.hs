module Lab3.Exercise5 where

import Data.List
import Lab3.Exercise4 (genDomainRelation)
import Lab3.SetOrd (Set (..), unionSet)
import System.Random
import Test.QuickCheck

-- Time Spent: 45 Minutes

{-
Exercise 5:
define a function that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., trClos
[(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] .

-}

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: (Eq a) => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- trClos will be used as a helper function by applying a sorting function to the pairs of transitive closure.
trClos :: (Ord a) => Rel a -> Rel a
trClos r = sortBy comparePairs $ trClos' r r

-- trClos' takes 2 sets which are the exact same for the first time it gets called,
-- Through the iterations the second set will get updated
trClos' :: (Ord a) => Rel a -> Rel a -> Rel a
trClos' r s
  | s' == s = s -- Base case
  | otherwise = trClos' r s' -- Iteration
  where
    s' = nub (s ++ (s @@ r))

-- CompareParis will sort the Set to get ordered like intended
comparePairs :: (Ord a) => (a, a) -> (a, a) -> Ordering
comparePairs (x1, y1) (x2, y2)
  | x1 == x2 = compare y1 y2
  | otherwise = compare x1 x2

-- Example usage
main :: IO ()
main = do
  let rel = [(1, 2), (2, 3), (3, 4)]
  print $ trClos rel

  (_, randRelation) <- generate (genDomainRelation)
  putStrLn $ "Randomly generated relation:" ++ (show randRelation)
  putStrLn $ "Gives symmetric closure: " ++ (show $ trClos randRelation)