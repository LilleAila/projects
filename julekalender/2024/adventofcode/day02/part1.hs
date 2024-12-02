import System.IO (readFile)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

diff :: Int -> Int -> Int
a `diff` b = abs $ a - b

diffRow :: [Int] -> [Int]
diffRow [x1, x2] = [x2 - x1]
diffRow (x1 : x2 : xs) = x2 - x1 : diffRow (x2 : xs)

validRow :: [Int] -> Bool
validRow row = withinBounds && (allPositive || allNegative)
  where
    withinBounds = all (\x -> abs x <= 3 && abs x >= 1) row
    allPositive = all (>= 0) row
    allNegative = all (< 0) row

main :: IO ()
main = do
  input <- readFile "input.txt"
  let input' = parseInput input
  let ans = length $ filter id $ map (validRow . diffRow) input'
  print ans
