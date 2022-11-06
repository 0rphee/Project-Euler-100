import Data.Maybe (isJust, fromJust)

isFactor :: Int -> Int -> Maybe Int
isFactor num x = if modulus == 0
                 then Just dividend
                 else Nothing
  where (dividend, modulus)= num `divMod` x

getFactors :: Int -> [Int]
getFactors num = concat [ a:[fromJust may] | a <- [1.. round (sqrt $ fromIntegral num)]
                        , let may = isFactor num a
                        , isJust may
                        ]

triangleNums :: [Int]
triangleNums = [ sum [1..a] | a <- [1..]]

result :: Int
result = head $ dropWhile 
  (\x -> (length . getFactors) x < 500) triangleNums

main :: IO ()
main = print result -- 76576500
