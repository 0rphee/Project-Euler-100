import Data.List (permutations, nub)

data Movement = MRight
              | MDown
              deriving (Eq, Show)

-- this will take decades to compute
combine nTimes ls = [ b | a <- replicate nTimes ls, b <- a ]
sol1 = (length . nub . permutations . combine 20) [MRight, MDown]

{- 
instead we use this formula to calculate the number of permutations
given n (total objetcs), p1 (number of objects of kind 1) and 
p2 (number of objects of kind 2)

combinations = n! / (p1! * p2!)
-}

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n-1)

nCombinations:: Integer -> Integer -> Integer
nCombinations xSize ySize = fact (xSize+ySize)
                            `div`
                            (fact xSize * fact ySize)

main :: IO ()
main = print $ nCombinations 20 20

