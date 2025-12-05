parseLine :: String -> Int
parseLine (x : n) = case x of
  'L' -> -d
  'R' -> d
  where
    d = read n

solve = length . filter (== 0) . scanl (\x d -> (x + d) `mod` 100) 50

main :: IO ()
main = do
  rotations <- map parseLine . lines <$> readFile "input.txt"
  print $ solve rotations
