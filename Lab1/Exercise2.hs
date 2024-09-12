import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = do
    li <- listGen 20
    putStrLn ("List: " ++ show li)
    let lengthLi = length li
    putStrLn ("Length list: " ++ show lengthLi)
    let lengthPower = length $ powerset li
    putStrLn ("Length power: " ++ show lengthPower)
    quickCheck (checkIntegrity lengthLi lengthPower)
    quickCheck (checkIfBigger lengthLi lengthPower)
    



powerset :: [a] -> [[a]]
powerset li
    | null li = [[]]
    |otherwise = subsequences li

listGen ::  Int ->IO[Int]
listGen n = replicateM n (randomRIO (1, 100))




--The length of elements in a powerset should be 2^n. n being the amount of values. The empty List should ALWAYS be present
checkIntegrity :: Int -> Int  -> Bool
checkIntegrity li pow = 2^li == pow

checkIfBigger :: Int -> Int -> Bool
checkIfBigger li pow = pow > li





    
