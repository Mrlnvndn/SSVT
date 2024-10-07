import Data.List
-- import System.Random
import Test.QuickCheck
import SetOrd (Set(..), inSet, list2set, unionSet)
-- import Exercise1 (quickCheckSetGen, randomSet)

{-
Time spent: 80 minutes
-}

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Function to compute the transitive closure of a relation
trClos :: Ord a => Rel a -> Rel a
trClos r = trClos' r []

-- Helper function to compute the transitive closure recursively
trClos' :: Ord a => Rel a -> Rel a -> Rel a
trClos' r prev
    -- Base case: if the current relation 'r' is equal to the previous relation 'prev', return 'r'
    | prev == r = r
    -- Recursive case: compute the new relation by adding pairs from 'r @@ r' to 'r'
    | otherwise =
        let newR = nub (r ++ (r @@ r))
        in trClos' newR r


main :: IO ()
main = do
    let r = [(1,2),(2,3),(3,4)]
    print $ trClos r