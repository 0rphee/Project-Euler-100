import Data.Char (digitToInt)

res num expo = sum [digitToInt a | a <- show (num^expo)]

main = print $ res 2 1000

