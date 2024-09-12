import Data.List
import Lecture2
import System.Random
import Test.QuickCheck

prop_evenAndLargerThan3 :: Int -> Bool
prop_evenAndLargerThan3 x = even x && x > 3

prop_evenOrLargerThan3 :: Int -> Bool
prop_evenOrLargerThan3 x = even x || x > 3

prop_evenAndLargerThan3OrEven :: Int -> Bool
prop_evenAndLargerThan3OrEven x = (even x && x > 3) || even x

prop_even :: Int -> Bool
prop_even = even

main :: IO ()
main = do
  let properties = [prop_evenAndLargerThan3, prop_evenOrLargerThan3, prop_evenAndLargerThan3OrEven, prop_even]
  let pairs = [(p1, p2) | p1 <- properties, p2 <- properties]
  print $ map (\(p1, p2) -> (compar [(-10) .. 10] p1 p2)) pairs