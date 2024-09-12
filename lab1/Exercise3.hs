import Data.List
import Test.QuickCheck
import Lecture2


{-
(\ x -> even x && x > 3) or even
(\ x -> even x || x > 3) or even
(\ x -> (even x && x > 3) || even x) or even
even or (\ x -> (even x && x > 3) || even x)

Prop1: (\ x -> even x && x > 3)
Prop2: (\ x -> even x || x > 3) 
Prop3: (\ x -> (even x && x > 3) || even x)
Prop4: (\ x -> (even x && x > 3) || even x)
PropEven: even

Answer: 
Prop1
PropEven
Prop4
Prop3
Prop2

Time spent: 120 min
-}

prop_one :: Integer -> Bool
prop_one x = even x && x > 3

prop_two :: Integer -> Bool
prop_two x = even x || x > 3

prop_three :: Integer -> Bool
prop_three x = (even x && x > 3) || even x

prop_four :: Integer -> Bool
prop_four x = (even x && x > 3) || even x

sortByStrength :: [(String, Integer -> Bool)] -> [(String, Integer -> Bool)]
sortByStrength [] = []
sortByStrength (x:xs) =
    sortByStrength [ a | a <- xs, weaker [-10..10] (snd x) (snd a) ]
   ++ [x]
   ++ sortByStrength [ a | a <- xs, not (weaker [-10..10] (snd x) (snd a)) ]



main :: IO()
main = do
    let props = [ ("Prop1", prop_one)
                , ("Prop2", prop_two)
                , ("Prop3", prop_three)
                , ("Prop4", prop_four)
                , ("PropEven", even)
                ]
    let sortedProps = sortByStrength props
    putStrLn "Sorted properties by strength:"
    mapM_ (putStrLn . fst) sortedProps



