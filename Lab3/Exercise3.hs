module Exercise3 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd ( Set(..), unionSet )

type Rel a = [(a,a)]

{-
    List comprehension to get the symmetric value of a pair by reorgonising the original tuple, concat to
    transform the list of lists (originated from the tuple manipulation) to a normal list and nub to get rid of any duplicates
    (which is the property of a set)
-}
symClos :: Ord a => Rel a -> Rel a
symClos rel = nub $ concat [[(x, y), (y, x)] | (x, y) <- rel]

{-
Example of a set which is going to become a symmetric closure of it. Could be done with a random Set
but the random generation of sets isn't the same approach for every team member. Therefore to ensure a 
proper functionnality of this file. A manually prepared set works best.
-}
main :: IO()
main = do
    let set = [(1,2),(2,3),(3,4)]
    print $ symClos set