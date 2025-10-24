fibonacciIndex limit n a b
  | b > limit = n
  | otherwise = fibonacciIndex limit (n + 1) b (a + b)

main :: IO ()
main = do
  print $ fibonacciIndex (10 ^ 999) 1 0 1
