import Data.List

xs :: [(Int, Int)]
xs = undefined

getFactors :: Int -> [Int]
getFactors num = helper num [2..num]
  where helper :: Int -> [Int] -> [Int]
        helper n (x:xs)
          | n `mod` x == 0 = x : helper (n `div` x) (x:xs)
          | otherwise = helper n xs
        helper _ [] = []
        
countFactorOcurrences :: [Int] -> [(Int, Int)]
countFactorOcurrences [] = []
countFactorOcurrences l = helper l []
  where helper :: [Int] -> [(Int, Int)] -> [(Int, Int)]
        helper [] ys = ys 
        helper (x:xs) [] = helper xs [(x, 1)]
        helper (x:xs) (y@(num,count):ys) 
          | x == num = helper xs ((num, count+1):ys)
          | otherwise = helper xs ((x,1):y:ys)
   

listOfFactors :: [Int]
listOfFactors = getFactors 20
countedF1 = countFactorOcurrences listOfFactors

listOfFactors' :: [Int]
listOfFactors' = getFactors 18
countedF2 = countFactorOcurrences listOfFactors'

jointFactors :: [(Int, Int)]
jointFactors = countedF1 ++ countedF2

getFactorsForMult :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getFactorsForMult [] a = a
getFactorsForMult (a:as) [] = getFactorsForMult as [a]
getFactorsForMult (a:as) bs = getFactorsForMult as (biggest:bs)
  where currentNums = filter (isNum a) bs
        biggest = foldl1 
                    (\(x,n1) (y,n2) -> if n1 > n2
                                       then (x,n1) 
                                       else (y,n2)) (a:currentNums)
        
isNum :: (Int, Int)-> (Int, Int) -> Bool        
isNum (num, _)(x, _) = num == x 

