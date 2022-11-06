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

main = print $ sum $ eratosSieve 2000000

