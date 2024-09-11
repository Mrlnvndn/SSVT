import Data.List
import System.Random
import Test.QuickCheck

-- import Lecture1
-- import Lecture2
-- import Lecture3

factorial :: Integer -> Integer
factorial input
  | input == 0 = 1
  | otherwise = factorial (input - 1) * input

prop_outputAlwaysBiggerThanZero :: Property
prop_outputAlwaysBiggerThanZero = forAll randomSingleOrDoubleDigitNumber ( \input -> factorial input > 0)

prop_outputAlwaysBiggerOrEqualToInput :: Property
prop_outputAlwaysBiggerOrEqualToInput = forAll randomSingleOrDoubleDigitNumber ( \input -> factorial input >= input )

--generating random single numbers
randomSingleOrDoubleDigitNumber :: Gen Integer
randomSingleOrDoubleDigitNumber =
  choose (1, 99)

main :: IO ()
main = do
  quickCheck prop_outputAlwaysBiggerOrEqualToInput
  quickCheck prop_outputAlwaysBiggerThanZero