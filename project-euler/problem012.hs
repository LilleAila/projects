-- Formula for number of factors taken from here: https://www.cuemath.com/numbers/factors/

import Data.List (group, sort)

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

triangleNumber :: Int -> Int
triangleNumber n = (n * (n + 1)) `div` 2

-- factors :: Int -> [Int]
-- factors n = [x | x <- [1 .. n], n `mod` x == 0]

factor :: Int -> [Int]
factor x = case filter (\p -> x `mod` p == 0) primes_lower of
  (y : _) -> y : factor (x `div` y)
  [] -> [x]
  where
    primes_lower = takeWhile (< x) primes

numberFactors :: Int -> Int
numberFactors x = product $ map (+ 1) factors
  where
    -- Sort: sort a list
    -- Group: group equal adjacent items into lists
    countFactors = map length . group . sort
    factors = countFactors $ factor x

main :: IO ()
main = do
  let triangleNumbers = [triangleNumber x | x <- [1 ..]]
  -- print $ head $ filter ((> 500) . length . factors) triangleNumbers
  print $ head $ filter ((> 500) . numberFactors) triangleNumbers
