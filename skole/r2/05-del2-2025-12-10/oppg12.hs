a :: Int -> Int
a n = n ^ 2 + 4 * n + 3

s :: Int -> Int
s n = sum $ map a [1 .. n]

main :: IO ()
main = do
  print $ s 13
  print $ (length . takeWhile (< 2000) . map s) [1 ..]
  print $ s 15
