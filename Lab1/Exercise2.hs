import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad
--Time spent: 180 Minutes (yeah i'm slow lol)

main :: IO ()
main = do
    let li = [intGen | x <- [1..1000]]
    quickCheck (testPowerSet li)

    



powerset :: [a] -> [[a]]
powerset  = subsequences 

--Random Integer generator with non specific range (could use stdGen to not specify any range)
intGen ::  IO Int
intGen = randomRIO (1, 10000)


--Testing the length of the power set compared to input set
testPowerSet :: [IO Int] -> Bool
testPowerSet li = 
    let lengthPower = length $ powerset li
        lengthList = length li
    in 2^lengthList == lengthPower
    
{-
    The cardinality property is itself not hard to test up to a certain extend (size of set). Once a set becomes too
    large the time complexity becomes a problem O(2^n). This number grows extremely fast

    In my opinion we are frist of all, checking if the property of powersets is respected in our program and demonstrate that 
        this "part-functionnality" works. We are testing an already established mathematical fact that helps us proove that 
        our function is correct (partly)
-}









    
