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
prop_areSameLength =
  forAll
    randomSingleDigitNumber
    ( \size ->
        conjoin [length perm === size | perm <- deran size]
    )

prop_areAllDifferent :: Property
prop_areAllDifferent =
  forAll
    randomSingleDigitNumber
    ( \size ->
        conjoin [allDifferent perm [1 .. size] === True | perm <- deran size]
    )

prop_haveSameElements :: Property
prop_haveSameElements =
  forAll
    randomSingleDigitNumber
    ( \size ->
        conjoin [sameElements perm [1 .. size] === True | perm <- deran size]
    )

randomSingleDigitNumber :: Gen Int
randomSingleDigitNumber =
  choose (0, 100)

-- prop_isAllDifferent

-- prop_hasSameElements
main :: IO ()
main = do
  quickCheck prop_areSameLength
  quickCheck prop_areAllDifferent
  quickCheck prop_haveSameElements