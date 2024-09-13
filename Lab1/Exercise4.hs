import Data.List
import System.Random
import Test.QuickCheck
import Text.ParserCombinators.ReadPrec (reset)


--Time Spent: 4 Hours
infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool -- Basically looks if prop is stronger by checking is restricts more
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p


-- Check if one list is a derangement of another
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = length xs == length ys && all (\(x,y)-> x /=y) (zip xs ys)

-- Task 2: Generate all derangements of a list [0..n-1]
deran :: Int -> [[Int]]
deran n = filter (\xs -> isDerangement xs [0..n-1]) (permutations [0..n-1])


-- Property 1: A derangement has the same length as the original list
propSameLength :: [Int] -> Bool
propSameLength xs = all (\d -> length d == length xs) (deran (length xs))

-- Property 2: No element in a derangement is in its original position
propNoElementInSamePosition :: [Int] -> Bool
propNoElementInSamePosition xs = all (\d -> isDerangement d xs) (deran (length xs))

--Evaluating the strength of the properties and retruning a string with strongest in first position
propertyStrength :: [Int] -> [String]
propertyStrength xs
  | stronger [xs] propNoElementInSamePosition propSameLength  = ["1. Property of element not in the same Position", "2. Property of same length"]
  | otherwise = ["1. Property of same length", "2. Property of element not in the same Position"]

--random List generator of 9 elements
generateRandomList :: IO [Int]
generateRandomList = mapM (\_ -> randomIO) [1..9]

  -- QuickCheck tests, 9 Elements in a list in the max to be able to run the program smoothy. Time complexity is really high.
main :: IO ()
main = do
  li <- generateRandomList
  quickCheck (propSameLength li)
  quickCheck (propNoElementInSamePosition li)








 



    