import Data.List (transpose)
import System.IO (readFile)

type WordRow = [Char]

type WordSearch = [WordRow]

parseInput :: String -> WordSearch
parseInput = lines

rowOccurences' :: String -> WordRow -> Int
rowOccurences' target row
  | length row < length target = 0
  | take (length target) row == target = 1 + rowOccurences' target (tail row)
  | otherwise = rowOccurences' target (tail row)

rowOccurences :: String -> WordRow -> Int
rowOccurences target row = rowOccurences' target row + rowOccurences' target (reverse row)

-- https://stackoverflow.com/a/32469565
diagonals :: [[a]] -> [[a]]
diagonals = tail . go []
  where
    go b es_ =
      [h | h : _ <- b] : case es_ of
        [] -> transpose ts
        e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

allOccurences :: String -> WordSearch -> Int
allOccurences target grid = rows + cols + diagonals1 + diagonals2
  where
    rows = sum $ map (rowOccurences target) grid
    cols = sum $ map (rowOccurences target) (transpose grid)
    diagonals1 = sum $ map (rowOccurences target) (diagonals grid)
    diagonals2 = sum $ map (rowOccurences target) (diagonals (map reverse grid))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let input' = parseInput input
  print $ allOccurences "XMAS" input'
