import Data.List (sort)

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = [x : ps | x <- xs, ps <- permutations (filter (/= x) xs)]

main :: IO ()
main = do
  print $ foldl (\a x -> a * 10 + x) 0 $ permutations [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] !! (10 ^ 6 - 1)
