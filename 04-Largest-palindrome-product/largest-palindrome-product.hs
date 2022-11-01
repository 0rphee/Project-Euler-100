{-# LANGUAGE TupleSections #-}

isPalindrome :: Int -> Bool
isPalindrome num = standard == reversed
  where standard = show num
        reversed = reverse standard
-- foldl :: (b -> a -> b) -> t a -> b 
data Result = Result Int (Int, Int)
  deriving (Show, Eq)

foldingFunc :: Maybe Result -> (Int, Int) -> Maybe Result
foldingFunc (Just (Result a (j, k))) (x, y)
  | isPalindrome num = if num > a
                       then Just (Result num (x, y))
                       else Just (Result a (j, k))
  | otherwise = Just (Result a (j, k))
  where num = x * y
foldingFunc Nothing (x, y)
  | isPalindrome num = Just $ Result num (x, y)
  | otherwise = Nothing
  where num = x * y

availableNums :: [Int]
availableNums = [100..999]

combine :: [Int] -> [Int] -> [(Int, Int)]
combine l1 l2 = concat ll
  where first = fmap (,) l1
        ll = fmap (\x -> fmap (,x) l1) l2

availableCombinations :: [(Int, Int)]
availableCombinations = combine availableNums availableNums

main :: IO ()
main = print $ foldl foldingFunc Nothing availableCombinations

-- Just (Result 906609 (993,913))
