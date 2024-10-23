import Data.List
import Lab1.Lecture2
import Lab1.Lecture3
import System.Random
import Test.QuickCheck

-- Time Spent: 60 Minutes
{-
    The main function that has to be created is IsPermutation which will check if two lists are permutations from each other
    A Permutation, unlike a derangement is a list based on an original one, which is going to have the exact same elements
    but at least 1 (so 2) elements have to have different indexes from the original list. Also they have to have the same length
-}

-- For a list to be a permutation of another one, it has to be of the same length and be non-identical in terms of indexes of values
isPermutation :: (Eq a) => [a] -> [a] -> Bool
isPermutation xs ys = iterations xs ys && ys /= xs && length xs == length ys

-- Going through every element in the first list to check wether they are also present in the second list
iterations :: (Eq a) => [a] -> [a] -> Bool
iterations xs ys = all (`elem` ys) xs

-- If we can assume that there is no duplicates in our list then we can have the following properties to check:

-- Property states that 2 permutations of the same list, if ordered, will return the same List
sameList :: (Ord a) => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

-- Property states that permutations are symmetric, permutation of a list, then the list is also a permutation of the previous permutation
symmetry :: (Eq a) => [a] -> [a] -> Bool
symmetry xs ys = isPermutation xs ys == isPermutation ys xs

-- Generate a random list of 9 integers to automate the testing process, nub specifies to drop duplicates as assumed in the exercise.
-- 9 is the maximal range for optimal testing in terms of computation time. 10 is possible but will take way longer.
-- Permutations have a time complexity of 0(!n), also known as the worst time complexity, which means the computational time
-- grows in a factorial manner when the amount of elements in a list grows.
randomList :: IO [Int]
randomList = do
  list <- sequence [randomIO | _ <- [1 .. 9]]
  return (nub list)

-- Creating a permutation of the original random List and dropping the head because it gives us the original list
randomPermGen :: [Int] -> [[Int]]
randomPermGen xs = tail (permutations xs)

{-
    You may assume that your input lists do not contain duplicates. What does
    this mean for your testing procedure?

    This means that all possible list cases are going to work for my testing process.
    If I fed [2,1,2][1,2,3] to my function, then it would check if there is an element 1 and an element 2 in my second list
    Therefore it would assume that they are permuations even though they are not.
    Something that could be done about this is check for symmetry. In that case it would check the other way around and
    feed in [1,2,3][2,1,2]. In that case it will check if there is an element 1, 2 and 3 in the second list.
    This time it would return false as those elements do not appear in the second list.
    As we have to assume there is no duplicates, i will not my Symmetry function into the "ispermutation" to check for
    duplicates as this will cause "isPermutations" to enter an endless loop of switching in between the two lists to check
    for symmetry.
    I will therefore make sure the random list generator doesn't produce any duplicate elements in the list.

    It is tricky to automate the testing process as Quickcheck can't automatically create permutation sof specific list
    it has to get fed already established permutations of a specific list. It then needs to run through 100 automatically created
    permutations and compare it to the original List.
-}

main :: IO ()
main = do
  -- List of well chosen Lists to test that respect basic properties of permutations
  -- Returns true, different from derangements
  print (isPermutation [1, 2, 3] [1, 3, 2])
  -- returns False because length is not the same
  print (isPermutation [1, 2, 3] [1, 3, 2, 4])
  -- returns False because they're the same lists
  print (isPermutation [1, 2, 3] [1, 2, 3])
  -- Assuming there IS duplicates, returns true even though it is not a permutation per se
  print (isPermutation [2, 1, 2] [1, 2, 3])

  -- Automation of the quickcheck processes
  randomList' <- randomList
  let randomPerm = randomPermGen randomList'
  -- Runs automated quickcheck test on sameList property
  quickCheck $ forAll (elements randomPerm) (sameList randomList')
  -- Automated test for symmetry property
  quickCheck $ forAll (elements randomPerm) (symmetry randomList')
  -- Automated test for isPermutation
  quickCheck $ forAll (elements randomPerm) (isPermutation randomList')