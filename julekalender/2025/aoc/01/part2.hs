parseLine :: String -> Int
parseLine (x : n) = case x of
  'L' -> -d
  'R' -> d
  where
    d = read n

solve :: Int -> [Int] -> Int
solve _ [] = 0
solve x (d : ds) = n + solve (x' `mod` 100) ds
  where
    x' = x + d
    n = (length . filter (== 0) . map (`mod` 100)) (drop 1 [x, x'' .. x'])
    x'' = if x' < x then x - 1 else x + 1

main :: IO ()
main = do
  rotations <- map parseLine . lines <$> readFile "input.txt"
  print $ solve 50 rotations
