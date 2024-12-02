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
  let tverrsum_primes = [prime | x <- [0 ..], let prime = primes !! x, tverrsum (x + 1) == tverrsum prime]
  let answer = sum $ take 10000 tverrsum_primes
  print answer
