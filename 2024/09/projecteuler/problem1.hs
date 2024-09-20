-- Project euler problem 1
-- Sum of numbers below 1000 divisible by 3 or 5

multiples :: [Int] -> Int -> [Int]
multiples divisors max = [x | x <- [1 .. (max - 1)], any (\d -> x `mod` d == 0) divisors]

main :: IO ()
main = do
  print $ sum $ multiples [3, 5] 1000
