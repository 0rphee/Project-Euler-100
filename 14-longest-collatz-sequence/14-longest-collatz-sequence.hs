
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n : collatz next
  where next = if even n
               then n `div` 2
               else 3*n + 1

--         [(chainLenght, initialNumber)]
numbers :: [(Int, Int)]
numbers = [(length $ collatz n, n) | n <- [1..1000000]]

main :: IO () -- (525, 837799)
main = print $ maximum numbers

