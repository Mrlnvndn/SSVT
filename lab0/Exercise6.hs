
{-
Find a prime number 
Add it to the previous prime number
Check if count <= 101
    continue
    else end
-}

-- Reusing a function to find a prime number from exercise 4
findPrime :: Integer -> Bool
findPrime i
    | i < 2 = False
    | otherwise =
        let len = length [k | k <- [1..floor $ sqrt $ fromIntegral i], mod i k == 0]
        in len == 1

{-
This function is not performing correctly, it is getting stuck in an infinite loop
The goal 
-}
listOfPrime :: Integer -> [Integer] -> [Integer]
listOfPrime val pList
    | length pList == 101 && findPrime (sum pList) = pList
    | length pList > 101 = listOfPrime val (take 101 pList)
    | otherwise =
        if findPrime val
            then listOfPrime (val + 1) (pList ++ [val])
        else listOfPrime (val+1) pList
    -- [k | k <- [lower..lower+600], findPrime k]

checkSum :: Integer -> [Integer] -> [Integer]
checkSum start primes
    | findPrime $ sum primes = primes
    | otherwise = checkSum (start + 1) (listOfPrime (start + 1) primes)

consecutive101Prime :: [Integer]
consecutive101Prime =
    checkSum 0 $ listOfPrime 0 []
