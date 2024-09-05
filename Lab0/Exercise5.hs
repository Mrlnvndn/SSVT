module Exercise5 where


import Data.List
import Data.Maybe
import GHC.Base

-- Time spent: 45 min

{-
Exercise: Describe and implement ROT13 encoding

Spec for ROT13 encoding (from wikipedia: https://en.wikipedia.org/wiki/ROT13):
- ROT13 is a simple letter substitution cipher that replaces each letter with the 13th letter after it alphabetically.
- If the end is reached the alphabet is wrapped around.
- ROT13 is its own inverse, which means you can reverse the encoding by applying the algorithm again (alphabet has 26 letters).

-}

alphabet :: [Char]
alphabet = ['a'..'z']

-- Helper function to calculate distance within the alphabet for indices (not always valid, but good enough to test for 13)
alphDistance :: Char -> Char -> Int
alphDistance v1 v2 = do 
    let diff = (alphIndex v1) - (alphIndex v2)
    if diff < 0 then diff + 26 else diff;

alphIndex :: Char -> Int
alphIndex v = fromJust (elemIndex v alphabet)


rot13Char :: Char -> Char
rot13Char v = alphabet !! ((alphIndex v + 13) `mod` 26)

rot13 :: [Char] -> [Char]
rot13 l = map rot13Char l

-- Tests to check specification

checkDistanceProp :: Bool
checkDistanceProp = all (==13) [alphDistance a (rot13Char a) | a <- alphabet]

checkInverseProp :: Bool
checkInverseProp = rot13 (rot13 alphabet) == alphabet

main = do
    let res = rot13 alphabet
    print $ "Result for alphabet" ++ show res
    if checkDistanceProp
        then print "Success: Distance is 13 for all cases!"
        else print "Failed: Distance property doesn't hold!"
    if checkInverseProp
        then print "Success: ROT13 operation on list is reversible!"
        else print "Failed: Inverse property doesn't hold!"
