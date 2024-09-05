import Data.Char
import Data.List
import System.Random
import Test.QuickCheck

-- Exercise 2
-- Time Spent: 210 min
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n - 1)
  return (p : ps)

createSubsection :: [Float] -> (Float, Float) -> [Float]
createSubsection list (lower, upper) = [x | x <- list, x >= lower && x < upper]

calculatePercentage :: [Float] -> [Float] -> IO Float
calculatePercentage list sublist = do
  return (fromIntegral (length sublist) / fromIntegral (length list))

-- chose a range of 0.05, because it works
withinRange :: [Float] -> Bool
withinRange = all (\x -> x >= 0.25 - 0.25 * 0.05 && x <= 0.25 + 0.25 * 0.05)

testProbs :: Int -> IO Bool
testProbs n = do
  result <- probs n
  percentageBin1 <- calculatePercentage result (createSubsection result (0, 0.25))
  percentageBin2 <- calculatePercentage result (createSubsection result (0.25, 0.5))
  percentageBin3 <- calculatePercentage result (createSubsection result (0.5, 0.75))
  percentageBin4 <- calculatePercentage result (createSubsection result (0.75, 1))

  return (withinRange [percentageBin1, percentageBin2, percentageBin3, percentageBin4])

main :: IO ()
main = do
  result <- testProbs 100000
  print result