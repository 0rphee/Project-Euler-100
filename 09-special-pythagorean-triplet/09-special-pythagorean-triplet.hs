
nums :: [Int] 
nums = [ a*b*c | a <- [0..1000], b <- [0..1000-a], c <- [0..1000-a-b], a + b + c == 1000, a^2 + b^2 == c^2 ]

main :: IO ()
main = print nums

