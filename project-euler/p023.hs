-- WIP

import Data.List (nub)

limit = 28123

divisors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

abundant = [x | x <- [12 .. limit], sum (divisors x) > x]

abundantPairs = [x + y | x <- abundant, y <- abundant, x + y <= limit]

main :: IO ()
main = do
  let pairs = nub abundantPairs
  -- print $ take 10 abundantPairs
  -- print $ take 10 pairs
  print $ limit - length pairs
