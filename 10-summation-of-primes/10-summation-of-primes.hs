
getNPrimes :: Int -> [Int]
getNPrimes numOfPrimesToGet = helper 0 2 [2]
  where helper :: Int -> Int -> [Int] -> [Int]
        helper count carry primeList
          | count < numOfPrimesToGet = helper (count+1) (head nextList) nextList
          | otherwise = nextList
          where nextList = nextPrime primeList carry
 

getPrimesUntil :: Int -> [Int]
getPrimesUntil maxPrime = helper 2 [2]
  where helper :: Int -> [Int] -> [Int]
        helper carry primeList
          | nextCarry < maxPrime = helper nextCarry nextList
          | otherwise = tail nextList
          where nextList = nextPrime primeList carry
                nextCarry = head nextList


nextPrime :: [Int] -> Int -> [Int]
nextPrime [] _ = []
nextPrime primeList carry
  | any numIsFactor primeList = nextPrime primeList nextNum
  | otherwise = nextNum:primeList
  where nextNum = carry + 1
        numIsFactor y = nextNum `mod` y == 0
        
newNextP num = [ a | a <- [1..num], num `mod` a == 0 ]

        
main = print $ sum $ getPrimesUntil 2000000

