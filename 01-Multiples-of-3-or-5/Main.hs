
getSum :: Int -> Int
getSum belowN = h 0 0
  where h :: Int -> Int -> Int
        h current sum -- belowN 3
          | current == belowN = sum 
          | isMultiple current = h next (sum + current)
          | otherwise = h next sum
          where next = current + 1
        
isMultiple :: Int -> Bool
isMultiple n = mult3 || mult5
  where mult3 = n `mod` 3 == 0
        mult5 = n `mod` 5 == 0


main = print $ getSum 1000





