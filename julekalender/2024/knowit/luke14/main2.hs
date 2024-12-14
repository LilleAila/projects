countInt :: Int -> Int -> Int
countInt x y = (length . filter (== (head . show) y)) (show x)

printNumber :: [Int] -> Int -> [Int]
printNumber counts number = zipWith (-) counts (map (countInt number) [0 .. 9])

printNumbers :: [Int] -> [Int]
printNumbers numbers = foldr (\n ns -> printNumber ns (ns !! n)) numbers [0 .. 9]

main :: IO ()
main = do
  print $ printNumbers (replicate 10 100_000)
