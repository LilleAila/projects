import Data.Array (listArray, (!))

limit = 10000

divisors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

-- Using an Array speeds up the lookup to constant time.
d = listArray (1, limit) $ map (sum . divisors) [1 .. limit]

amicable a b = d ! a == b && d ! b == a

main :: IO ()
main = do
  -- let pairs = [x + y | x <- [1 .. limit], y <- [x + 1 .. limit], amicable x y]
  let pairs = [x + y | x <- [1 .. limit], let y = d ! x, y > x, y <= limit, d ! y == x]
  print $ sum pairs
