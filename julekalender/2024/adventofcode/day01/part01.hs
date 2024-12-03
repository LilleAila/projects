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

sortLists :: Lists -> Lists
sortLists (a, b) = (sort a, sort b)

differences :: Lists -> [Int]
differences (list_a, list_b) = [abs (a - b) | (a, b) <- zip list_a list_b]

solve :: Lists -> Int
solve lists = sum $ differences $ sortLists lists

main :: IO ()
main = do
  input <- readFile "input.txt"
  let input' = parseInput input
  print $ solve input'
