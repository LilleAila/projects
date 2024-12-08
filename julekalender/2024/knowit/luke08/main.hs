import Data.Char (digitToInt)
import Data.List (transpose)

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

isPrime :: Int -> Bool
-- isPrime n = n `elem` takeWhile (<= n) primes
isPrime n = n == last (takeWhile (<= n) primes)

multiplyPowers :: Int -> [[Int]] -> [[Int]]
multiplyPowers n = zipWith (map . (*)) powers
  where
    powers = map (n ^) [0 ..]

primtalv :: [Int]
primtalv = map primtalv' [1 ..]
  where
    primtalv' :: Int -> Int
    primtalv' n = result
      where
        primeNums = take n primes
        primeDigits = (multiplyPowers 10 . map (map digitToInt) . transpose . map show) primeNums
        result = (sum . map sum) primeDigits

perfektePrimtalv :: [Int]
perfektePrimtalv = filter isPrime primtalv

main :: IO ()
main = do
  print $ (length . takeWhile (< 10_000_000)) perfektePrimtalv
