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


myElem :: Foldable t => (Int, Int) -> t (Int, Int) -> Bool
myElem = any . myEq
  where myEq (a, _) (x, _) = a == x

-- :: (b -> a -> b) -> [a] -> b

getFinalFactors :: [[(Int, Int)]] -> [(Int, Int)]
getFinalFactors = foldl1 foldingFun

foldingFun :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
foldingFun [] a = a
foldingFun a [] = a
foldingFun xs ys = helper xs ys
  where helper [] a = a
        helper a [] = a
        helper (a:as) bs =
          case item of
            Nothing -> a : helper as bs'
            Just w -> h a w : helper as bs'
          where item = myFind a bs
                h m@(_, t) n@(_, s)
                  | t > s = m                  
                  | otherwise = n
                bs' = filter (not . isNum a) bs
myFind :: Foldable t => (Int, Int) -> t (Int, Int)-> Maybe (Int, Int)
myFind y = find (isNum y)

isNum :: (Int, Int)-> (Int, Int) -> Bool
isNum (num, _)(x, _) = num == x


lOfLOfFactors20 :: [[(Int, Int)]]
lOfLOfFactors20 = [(countFactorOcurrences . getFactors) a | a <- [1..20]]

finalLOfFactors :: [(Int, Int)]
finalLOfFactors = getFinalFactors lOfLOfFactors20

prodFactorsFolder :: Int -> (Int, Int) -> Int
prodFactorsFolder a (val, expon) = a * (val ^ expon)

main = print $ foldl prodFactorsFolder 1 finalLOfFactors

