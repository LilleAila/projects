-- Improved with help from https://chatgpt.com/share/674d96db-5c2c-800d-8a37-5ed6d198246d

import Data.Char (digitToInt)

-- Taken from the wiki: https://wiki.haskell.org/index.php?title=Prime_numbers&oldid=36858#Postponed_Filters_Sieve
primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

tverrsum :: Int -> Int
tverrsum num = sum (map digitToInt (show num))

main :: IO ()
main = do
  let tverrsumPrimes = [prime | (prime, pos) <- zip primes [1 ..], tverrsum prime == tverrsum pos]
  let answer = sum $ take 10000 tverrsumPrimes
  print answer
