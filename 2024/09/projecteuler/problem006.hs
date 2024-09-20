squareSumDifference :: [Int] -> Int
squareSumDifference xs = sumSquares xs - squareSum xs
  where
    squareSum xs = sum xs ^ 2
    sumSquares xs = sum [x ^ 2 | x <- xs]

main :: IO ()
main = do
  print $ abs $ squareSumDifference [1 .. 100]
