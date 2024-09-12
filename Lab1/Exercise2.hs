import Data.List
import System.Random
import Test.QuickCheck

powerset :: [a] -> [[a]]
powerset = subsequences

prop_powersetInduction :: Property
prop_powersetInduction = forAll genListOfIntegers (\xs -> length (powerset xs) == length (powerset (tail xs)) * 2)

prop_powersetSize :: Property
prop_powersetSize = forAll genListOfIntegers (\xs -> length (powerset xs) == 2 ^ length xs)

-- Generator for a list of integers
genListOfIntegers :: Gen [Integer]
genListOfIntegers = listOf arbitrary `suchThat` (\xs -> length xs > 1 && length xs < 10)

main :: IO ()
main = do
  quickCheck prop_powersetInduction
  quickCheck prop_powersetSize