-- Taken from the wiki: https://wiki.haskell.org/index.php?title=Prime_numbers&oldid=36858#Postponed_Filters_Sieve
-- It's an implementation of the Sieve of Eratosthenes that can be lazily evaluated by haskell
primes :: [Integer]
-- Initialize with 2 and 3 as primes, and only check odd numbers
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    -- Return a list of h, the verified primes
    -- and continue checking higher numbers recursively
    -- rem is like mod but faster and works for positive numbers
    -- it basically takes all non-checked possible primes and filters out those divisible by p
    -- p is the first prime in the list of verified primes, while ps is the rest of the primes
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        -- ~ Lets the rest be lazily evaluated
        -- By now, all numbers below p^2 will be primes
        -- , because they've already been checked against the lower numbers
        -- span returns a tuple of (matched, not matched) for the function passed
        (h, ~(_ : t)) = span (< p * p) xs

main :: IO ()
main = do
  print $ sum $ takeWhile (< 2000000) primes
