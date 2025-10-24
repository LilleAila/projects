import Data.List (maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

limit = 999

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

coeffs = [(x, y) | x <- [-limit .. limit], y <- [x .. limit]]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\p -> n `rem` p /= 0) $ takeWhile (\p -> p * p <= n) primes

numPrimes x y = length $ takeWhile isPrime (map formula [0 ..])
  where
    formula n = n ^ 2 + x * n + y

main :: IO ()
main = do
  let nums = zip coeffs $ map (uncurry numPrimes) coeffs
  let ((a, b), _) = maximumBy (comparing snd) nums
  print $ a * b
