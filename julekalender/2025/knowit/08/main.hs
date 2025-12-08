{-# OPTIONS_GHC -Wno-x-partial #-}

primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\p -> n `rem` p /= 0) $ takeWhile (\p -> p * p <= n) primes

digits :: Integer -> [Integer]
digits = map (read . (: [])) . show

-- True if the input is a "harshad" number and the quotient is a prime number
gledesspreder :: Integer -> Bool
gledesspreder x = x `mod` ds == 0 && isPrime (x `div` ds)
  where
    ds = sum $ digits x

main :: IO ()
main = do
  numbers :: [Integer] <- map read . words <$> readFile "input.txt"
  print $ length [x | x <- numbers, gledesspreder x]
