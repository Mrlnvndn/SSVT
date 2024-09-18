import Data.List
import System.Random
import Test.QuickCheck


--For a list to be a permutation of another one, it has to be of the same length and none of the elements can be at the same position of the other list
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `sameElems` ys &&
                        xs /= ys &&
                        length xs == length ys


sameElems :: Eq a => [a] -> [a] -> Bool
sameElems [] _ = True
sameElems xs ys = all (`elem` ys) xs


--If we can assume that there is no duplicates in our list then we can have the following properties to check:

--If we order two list of permutations, they should be the same List
sameList :: Ord a => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

symmetry :: Eq a => [a] -> [a] -> Bool
symmetry xs ys = isPermutation xs ys == isPermutation ys xs

randomElem :: [b] -> Gen b
randomElem l = do
    let n = length l
    i <- choose (0, n-1)
    return (l !! i)

-- Use quickcheck to test
genPermTuple :: Gen ([Int], [Int])
genPermTuple = do
    len <- choose(2,10)
    l <- vectorOf len (choose (1, 100))
    p <- randomElem $ tail $ permutations l
    return (l, p)


main :: IO()
main = do
    tuples <- generate genPermTuple
    print tuples

    quickCheck $ forAll genPermTuple $ uncurry isPermutation

    --List of well chosen lists that ARE permutations:
    let positiveTests = [
            isPermutation [1,2,3] [2,3,1],
            isPermutation [1,2,3,4] [4,3,2,1]
            ]
    if and positiveTests
        then putStrLn "Edge case permutations passed !"
        else putStrLn "Edge case permutations failed X"

    --Lists that are not permutations
    let negativeTests = [
            isPermutation [1..25] [1..25], -- testing same list (not permuted)
            isPermutation [1,2,3] [2,2,0],
            isPermutation [1..10] (init [1..10]) -- testing wrong length
            ]
    if not $ and negativeTests
        then putStrLn "Tests non-permutations passed !"
        else putStrLn "Tests non-permutations failed X"

{-
    You may assume that your input lists do not contain duplicates. What does
    this mean for your testing procedure?
    This means that my permutations are easier to test but it wouldn't have altered my testing anyways

    It is very complicated to automate the testing process as quickcheck can't easily create permutations. 
-}

