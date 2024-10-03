module Exercise1 where

import Control.Monad
import Data.List
import GHC.IO.Device (IODevice (setSize))
import SetOrd
import System.Random
import Test.QuickCheck

generator :: IO (Set Int)
generator = do
  setSize <- randomRIO (1, 10)
  randomNumbers <- replicateM setSize (randomRIO (1, 10))
  return (list2set randomNumbers)

generator' :: Gen (Set Int)
generator' = do
  setSize <- choose (1, 10)
  randomNumbers <- vectorOf setSize (choose (1, 10))
  return (list2set randomNumbers)

main :: IO ()
main = do
  set <- generator
  putStrLn ("Scratch: " ++ show set)
  set2 <- generate generator'
  putStrLn ("Quickcheck: " ++ show set2)