primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

factor :: Int -> [Int]
factor x = case filter (\p -> x `mod` p == 0) primes_lower of
  (y : _) -> y : factor (x `div` y)
  [] -> [x]
  where
    primes_lower = takeWhile (< x) primes

main :: IO ()
main = do
  print $ maximum $ factor 600851475143
