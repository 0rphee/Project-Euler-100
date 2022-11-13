digits = [1..9]
digitsName = [ "one", "two", "three"
             , "four", "five", "six"
             , "seven", "eight", "nine" ]
digitsNameLength :: [Int]
digitsNameLength = length <$> digitsName

special = [11..19]
specialName = [ "eleven", "twelve", "thirteen"
          , "fourteen", "fifteen", "sixteen"
          , "seventeen", "eighteen", "nineteen"]
specialNameLengths :: [Int]
specialNameLengths = length <$> specialName


tens = tail [0,10..90]
tensName = [ "ten", "twenty", "thirty"
           , "forty", "fifty", "sixty"
           , "seventy", "eighty", "ninety"]
tensNameLenght :: [Int]
tensNameLenght = length <$> tensName

decenasNameLength :: [Int] -- ["twentyone"..."ninentynine"]
decenasNameLength = [ a + b | a <- digitsNameLength, b <- tail tensNameLenght]

hundredsNameLength :: [Int] -- [length "onehundred", ...]
hundredsNameLength = (+ length "hundred") <$> digitsNameLength

betweenHundredsLengths :: [Int] -- [length "one"...length "ninetynine"]
betweenHundredsLengths = digitsNameLength ++ decenasNameLength ++ specialNameLengths ++ tensNameLenght

oneTo100 :: Int -- sumatory of 1-99 
oneTo100 = sum betweenHundredsLengths -- add composed names

between2Hundreds :: Int -- adding and's, to generalize to x01-x99
between2Hundreds = oneTo100 + (length "and" * length betweenHundredsLengths)

betweenHundredsSum :: Int -- sumatory of x01-x99, multiplied for every "hundred" 
betweenHundredsSum = between2Hundreds * 9

hundredsSum :: Int -- ["onehundred", ...]* Number of appearences -> x00-x99
hundredsSum = sum ((*100) <$> hundredsNameLength)

totalLength :: Int
totalLength = betweenHundredsSum + hundredsSum + oneTo100 + length "onethousand"

main :: IO ()
main = print totalLength





