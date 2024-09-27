
import Data.List
import Test.QuickCheck

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =
    let ps = powerset xs 
    in ps ++ map (x:) ps

{-
1. Is the cardinality property of the powerset hard to test? If you find that it is, can you give a
reason why?

It is not technically hard to test, it is a relatively easy function to implement. However, it is hard
to test the cardinality property because there are an infinite number of finite sets, which makes it 
impossible to test all possible inputs. The cardinality property is itself not hard to test up to a 
certain extend (size of set). Once a set becomes too large the time complexity becomes a problem O(2^n). 
This number grows extremely fast

So to prove this property, you would need to use a proof by induction.

2. Give your thoughts on the following issue: When you perform these tests, what are you
testing actually? Are you checking a mathematical fact? Or are you testing whether
powerset satisfies a part of its specification? Or are you testing something else still?

What you are testing when runnning the tests largely depends on what you rely on. For example,
if I proved a property of powerset to be true through induction, you could then test the correctness
of your powerset implementation by checking if its results satisfy this property. Similairly, you 
could test the implementation of your powerset function by testing its output to haskell's subsequences
function. 


I am using a list size of 20 for computational reasons, the programs runs for a very long time 
with a higher number.

Time spent: 70 minutes
-}

prop_card :: [Integer] -> Bool 
prop_card s =
    let n = length s 
    in length (powerset s) == 2 ^ n

genSmallList :: Gen [Integer]
genSmallList = 
    sized $ \n -> do
    k <- choose (0, 20)  
    vectorOf k arbitrary

main :: IO()
main = do
    quickCheck (forAll genSmallList prop_card )