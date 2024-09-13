import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Property

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool -- Basically looks if prop is stronger by checking is restricts more
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

    
    
    

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

