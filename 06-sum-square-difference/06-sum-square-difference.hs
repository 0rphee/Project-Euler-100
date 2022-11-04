
sumSquares = sum [ a ^ 2 | a <- [1..100]]
squaredSum = sum [1..100] ^ 2

main = print $ squaredSum - sumSquares
  
