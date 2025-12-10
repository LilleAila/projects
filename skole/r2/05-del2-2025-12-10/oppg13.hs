a :: Float -> Float
a n = pi * 25 * (0.7 ** (4 * n - 4) - 0.7 ** (4 * n - 2))

main :: IO ()
main = do
  let ks = [a (n + 1) / a n | n <- [1 .. 10]]
  let usikkerhet = (maximum ks - minimum ks) / 2
  let avg = sum ks / fromIntegral (length ks)
  print ks
  putStrLn $ "k = " ++ show avg ++ "±" ++ show usikkerhet
  putStrLn $ "a1 = " ++ show (a 1)
  print $ sum . takeWhile (> 0) . map a $ [1 ..]
