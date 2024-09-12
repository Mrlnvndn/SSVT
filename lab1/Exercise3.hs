import Data.List
import Test.QuickCheck
import Lecture2



-- stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
-- stronger xs p q = forall xs (\ x -> p x --> q x)
-- weaker xs p q = stronger xs q p

{-
(\ x -> even x && x > 3) or even
(\ x -> even x || x > 3) or even
(\ x -> (even x && x > 3) || even x) or even
even or (\ x -> (even x && x > 3) || even x)
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
    -- let props = [("Property One", \ x -> even x && x > 3), ("Property Two", \ x -> even x || x > 3), ("Property Three", \ x -> (even x && x > 3) || even x), ("Property Four", \ x -> (even x && x > 3) || even x), ("Even Property", even)]
    -- sortByStrength props\
    let props = [ ("Prop1", prop_one)
                , ("Prop2", prop_two)
                , ("Prop3", prop_three)
                , ("Prop4", prop_four)
                , ("PropEven", even)
                ]
    let sortedProps = sortByStrength props
    putStrLn "Sorted properties by strength:"
    mapM_ (putStrLn . fst) sortedProps



