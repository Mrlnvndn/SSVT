module Exercise3 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd ( Set(..), unionSet )
import Exercise1 (manualMethod, quickMethod, generateRandomValue)

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos rel = nub $ concat [[(x, y), (y, x)] | (x, y) <- rel]

main :: IO()
main = do
    let set = [(1,2),(2,3),(3,4)]
    print $ symClos set