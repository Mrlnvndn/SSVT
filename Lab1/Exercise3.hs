import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property
import Lecture2


    
    
    

firstProp :: Int -> Bool
firstProp n = even n && n > 3

secondProp :: Int -> Bool
secondProp n =  even n || n > 3

thirdProp :: Int -> Bool
thirdProp n = (even n && n > 3) || even n

fourthProp :: Int -> Bool
fourthProp n = even n || (even n && n>3)

evenProp :: Int -> Bool
evenProp  = even 

