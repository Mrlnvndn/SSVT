import Data.List
import LTS
import Test.QuickCheck
import System.Random


-- ltsGen :: Gen IOLTS
-- ltsGen =
    

genRandomInt :: Integer -> IO Integer
genRandomInt n = 
    randomRIO (1, n)