-- Project euler problem 2
-- Sum of even fibonacci numbers <= 4000000

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
  let numbers = map fibonacci [0 ..]
  let even_numbers = takeWhile (<= 4000000) [x | x <- numbers, even x]
  print $ sum even_numbers
