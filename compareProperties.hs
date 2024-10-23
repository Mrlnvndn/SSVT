import Test.QuickCheck

isCoreflexive :: Eq a => [(a,a)] -> Bool
isCoreflexive r = all (\(x,y) -> x == y) r

isTransitive :: Eq a => [(a, a)] -> Bool
isTransitive r = all (\(x,z) -> if existsY x z then (x,z) `elem` r else True) 
                    [(x,z) | (x,_) <- r, (_,z) <- r]
  where
    existsY x z = any (\y -> (x,y) `elem` r && (y,z) `elem` r) [y | (_,y) <- r]
    
isAsymmetric :: Eq a => [(a, a)] -> Bool
isAsymmetric r = all (\(x,y) -> not $ (y,x) `elem` r) r


prop_coreflexive_and_transitive :: [(Int, Int)] -> Property
prop_coreflexive_and_transitive r =
  (isCoreflexive r && isTransitive r ==> isAsymmetric r && isTransitive r)
  
prop_asymmetric_and_transitive :: [(Int, Int)] -> Property
prop_asymmetric_and_transitive r =
  isAsymmetric r && isTransitive r ==> isCoreflexive r && isTransitive r


main :: IO ()
main = do
  quickCheck prop_coreflexive_and_transitive
  quickCheck prop_asymmetric_and_transitive
