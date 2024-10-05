-- Too slow
{-
numPaths :: Int -> Int
numPaths = gridPaths 0 0
  where
    gridPaths x y n
      | x == n && y == n = 1
      | x == n = gridPaths x (y + 1) n
      | y == n = gridPaths (x + 1) y n
      | otherwise = gridPaths (x + 1) y n + gridPaths x (y + 1) n
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

-- n is the width of the board
-- 2 * n is the total number of steps required
-- n is the number of down / right steps required
-- (they're equal because the board is square)
-- This means that there are different combinations of R and D, of length 4, with 2 R's and 2 D's
-- All steps that are not R have to be D, so in practice, this is finding the number of ways R can be placed in a sequence of length 4
-- The binomial coefficient is used for this: https://no.wikipedia.org/wiki/Binomialkoeffisient
numPaths :: Integer -> Integer
numPaths n = factorial (2 * n) `div` (factorial n * factorial n)

main :: IO ()
main = do
  print $ numPaths 20
