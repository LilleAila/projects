import Data.List (sort)
import System.IO (readFile)

type Lists = ([Int], [Int])

parseInput :: String -> Lists
parseInput input = foldl (\(acc_a, acc_b) [a, b] -> (a : acc_a, b : acc_b)) ([], []) $ parseInput' input
  where
    parseInput' :: String -> [[Int]]
    parseInput' = map parseLine . lines
      where
        parseLine = map read . words

scores (list_a, list_b) = [x * occurences x list_b | x <- list_a]
  where
    occurences x ys = length $ filter (== x) ys

solve :: Lists -> Int
solve lists = sum $ scores lists

main :: IO ()
main = do
  input <- readFile "input.txt"
  let input' = parseInput input
  print $ solve input'
