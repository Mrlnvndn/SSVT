module Exercise3 where

import Data.Set(toList, fromList)
import Data.Tuple
import Data.List
type Rel a = [(a,a)]

-- Time Spent 15 min

{--
Exercise 3: 
Create the symmetric closure of a relation R (represented by a list of tuples).

A clean way to calculate this is by doing a union over R and its converse relation R^T
https://en.wikipedia.org/wiki/Symmetric_closure

To get the converse we simply swap each tuple in the relation. To ensure we have no duplicates
we convert the list to a set and then back.
Sorting is not strictly necessary, but makes the result easier to compare.

--}

listUnique :: Ord a => [a] -> [a]
listUnique = toList . fromList

converseRel :: Ord a => Rel a -> Rel a
converseRel rel = map swap rel

symClos :: Ord a => Rel a -> Rel a
symClos rel = sort $ listUnique $ rel `union` (converseRel rel)


main = do
    let testRel = [(1,2),(2,3), (3,2), (3,4)]
    print $ sort $ symClos testRel