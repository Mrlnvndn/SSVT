import Data.List
import Lecture2
import System.Random
import Test.QuickCheck

isDerangement :: (Eq a) => [a] -> [a] -> Bool
isDerangement xs ys = sameLength xs ys && allDifferent xs ys && sameElements xs ys

sameLength :: [a] -> [a] -> Bool
sameLength xs ys = length xs == length ys

allDifferent :: (Eq a) => [a] -> [a] -> Bool
allDifferent xs ys = all (\(x, y) -> x /= y) (zip xs ys)

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

deran :: Int -> [[Int]]
deran 0 = [[]]
deran n = [perm | perm <- permutations [1 .. n], allDifferent [1 .. n] perm]

prop_areSameLength :: Property
prop_areSameLength = forAll randomSingleDigitNumber (\size -> forAll (elements (deran size)) (\perm -> length perm == size))

randomSingleDigitNumber :: Gen Int
randomSingleDigitNumber =
  choose (2, 9)

-- prop_isAllDifferent

-- prop_hasSameElements
main :: IO ()
main = do
  quickCheck prop_areSameLength