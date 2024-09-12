import Data.List
import System.Random
import Test.QuickCheck
import Text.ParserCombinators.ReadPrec (reset)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement li1 li2 = null [x | li1 == li2, x <- li1]



deran :: Int -> [[Int]] 
deran n = filter (isDeran n) (permutations [n-1])



isDeran :: Int -> [Int] -> Bool
isDeran n perm = result where
    original = [0..n]
    result = all (\(x,y) -> x /= y) (zip original perm)



main :: IO ()
main = do
    let n = 5
    let listy = [1..n]
    let derangedList = deran n
    print derangedList



    