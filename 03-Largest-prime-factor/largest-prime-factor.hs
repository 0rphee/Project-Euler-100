num = 600851475143 :: Int

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
        
nxPrime :: [Int] -> [Int]
nxPrime [] = [2]
nxPrime (x:xs) = nextPrime (x:xs) x

main = print $ getPrimesUntil num

-- get primes up to a number n with erastosthenes sieve

eratosSieve :: Int -> [Int]
eratosSieve num = h [2..num] 0
  where h :: [Int] -> Int -> [Int]
        h [] _ = []
        h ll nDrop
          | y^2 <= num = h (prev ++ x:y:ys ) (nDrop+1)
          | otherwise = prev ++ x:y:ys
          where (x:xs) = drop nDrop ll
                prev = take nDrop ll
                (y:ys) = filter (\w->w`mod`x/=0) xs


