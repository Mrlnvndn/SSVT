import Test.QuickCheck

numR :: Int -> Int -- reflexive
numR 0 = 1
numR n = 2^(2*n-2) * numR (n-1)

--Properties
{-
    Amount of relations from a Set = 2^(n^2)

    Amount of reflexive relations in a Set = 2^(n^2 - n)

    Amount of ireflexive relations in a Set = 2^(n^2 - n)
-}

-- Property 1: For n = 0, the number of reflexive relations should be 1
prop_num2022_base_case :: Bool
prop_num2022_base_case = numR 0 == 1

-- Property 2: For any n >= 1, the number of reflexive relations should be 2^(n^2 - n)
prop_num2022_correct :: Positive Int -> Bool
prop_num2022_correct (Positive n) = numR n == 2^(n^2 - n)

prop_increase :: Int -> Bool
prop_increase n = numR ( n + 1) > numR n 

main :: IO ()
main = do
  quickCheck prop_num2022_base_case
  quickCheck prop_num2022_correct
  quickCheck prop_increase


