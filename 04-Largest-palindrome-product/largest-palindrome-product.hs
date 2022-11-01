
main :: IO ()
main = print $ foldl fun Nothing final 

isPalindrome :: Int -> Int -> Bool
isPalindrome x y = standard == reversed
  where standard = show $ x * y
        reversed = reverse standard

availableNums :: [Int]
availableNums = [100..999]

-- :: (b -> a -> b) -> t a -> b 
data Result = Result Int (Int, Int)
  deriving (Show, Eq)

fun :: Maybe Result -> (Int, Int) -> Maybe Result
fun (Just (Result a (j, k))) (x, y)
  | isPalindrome x y = if (x*y) > a
                       then Just (Result (x*y) (x, y))
                       else Just (Result a (j, k))
  | otherwise = Just (Result a (j, k))
fun Nothing (x, y)
  | isPalindrome x y = Just $ Result (x*y) (x, y)
  | otherwise = Nothing

orderedList :: [(Int, Int)]
orderedList = zip availableNums availableNums

ordRev = zip availableNums (reverse availableNums)

final = orderedList ++ ordRev




