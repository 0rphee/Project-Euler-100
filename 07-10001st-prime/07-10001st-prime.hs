
nextPrime :: [Int] -> Int -> [Int]
nextPrime [] _ = []
nextPrime primeList carry
  | any numIsFactor primeList = nextPrime primeList nextNum
  | otherwise = nextNum:primeList
  where nextNum = carry + 1
        numIsFactor y = nextNum `mod` y == 0

getNPrimes :: Int -> [Int]
getNPrimes numOfPrimesToGet = helper 0 2 [2]
  where helper :: Int -> Int -> [Int] -> [Int]
        helper count carry primeList
          | count < numOfPrimesToGet = helper (count+1) (head nextList) nextList
          | otherwise = nextList
          where nextList = nextPrime primeList carry
 
main = do
  let a = getNPrimes 10001
  print $ head a
  print $ length $ getNPrimes 10001        
