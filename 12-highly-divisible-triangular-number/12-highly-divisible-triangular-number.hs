
isFactor :: Int -> Int -> (Int, Bool)
isFactor num x = (dividend, modulus == 0)
  where (dividend, modulus) = num `divMod` x

getFactors :: Int -> [Int]
getFactors num = concat [ a:[dividend] | a <- [1.. round (sqrt $ fromIntegral num)]
                        , let (dividend, factor)= isFactor num a
                        , factor
                        ]

triangleNums :: [Int]
triangleNums = [ sum [1..a] | a <- [1..]]

result :: Int
result = head $ dropWhile 
  (\x -> (length . getFactors) x < 500) triangleNums

main :: IO ()
main = print result -- 76576500
