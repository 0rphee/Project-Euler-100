num = 600851475143 :: Int

main :: IO ()
main = print $ maximum (getPrimeFactors num) 


{-
[18:22:11] 
❱❱❱ ./largest-prime-factor
6857             <----------------------------------------------- actual result, 1.2016283333 hours lol
[cost 4325.862s] [19:34:19] ./largest-prime-factor
-}

-- get primes up to a number n with erastosthenes sieve

getPrimeFactors :: Int -> [Int]
getPrimeFactors num = helper num [2..num]
  where helper :: Int -> [Int] -> [Int]
        helper _ [] = []
        helper n (x:xs)
          | modulus == 0 = x : helper divisor (x:xs)
          | otherwise = helper n xs
          where (divisor, modulus) = n `divMod` x
          
newGetPF num = [a | a <- eratosSieve num , num `mod` a == 0 ]          


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
                

