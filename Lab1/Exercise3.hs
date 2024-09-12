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

{-
• (\ x -> even x && x > 3) or even
• (\ x -> even x || x > 3) or even
• (\ x -> (even x && x > 3) || even x) or even
• even or (\ x -> (even x && x > 3) || even x)

1. Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type
Int -> Bool . Consider a small domain like [(−10)..10]
2. Provide a descending strength list of all the implemented properties.
-}
main :: IO ()
main = do
    --let checkFirstLine = [compareStrength x (firstProp x)(evenProp x) | x<- [-10..10] ]
    let li = [-10..10]
    {-let firstComp = compareStrength li firstProp evenProp
    let secondComp = compareStrength li secondProp evenProp
    let thirdComp = compareStrength li thirdProp evenProp
    let fourthComp = compareStrength li evenProp fourthProp 
    
    print firstComp
    print secondComp
    print thirdComp
    print fourthComp-}

    print compareAll
    
    
    

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



   
    


{-compareStrength :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compareStrength xs p q = let pq = stronger xs p q
                             qp = stronger xs q p
                in
                    if pq && qp then "equivalent"
                    else if pq then "stronger"
                    else if qp then "weaker"
                    else "incomparable"
-}
