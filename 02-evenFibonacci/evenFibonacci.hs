-- Even Fibonacci numbers
fourMil = 4000000

fibNum :: Int -> Int -> Int -> Int
fibNum x y stop = helper 1 2 0 stop
  where helper :: Int -> Int -> Int -> Int -> Int
        helper x y count stop
          | count >= stop = y
          | otherwise = helper y next (count+1) stop
          where next = x + y
          
fibNumList :: Int -> Int -> Int -> [Int]
fibNumList x y stop = x:y: helper 1 2 0 stop
  where helper :: Int -> Int -> Int -> Int -> [Int]
        helper x y count stop
          | count >= stop = [next] 
          | otherwise = next :helper y next (count+1) stop
          where next = x + y

fibListUpToNum :: Int -> Int -> Int -> [Int]
fibListUpToNum x y stop = x:y: helper 1 2 stop
  where helper :: Int -> Int -> Int -> [Int]
        helper x y stop
          | stop <= next = [] 
          | otherwise = next : helper y next stop
          where next = x + y
getSum :: [Int] -> Int
getSum list = sum filList
  where filList = filter even list

resultList = fibListUpToNum 1 2 fourMil

filteredList = filter even resultList

sumResult = getSum filteredList

