-- Simple brute-force sudoku solver algorithm using backtracking

import Data.List

type Row = [Int]

type Grid = [Row]

game1 :: Grid
game1 =
  [ [0, 0, 0, 2, 6, 0, 7, 0, 1],
    [6, 8, 0, 0, 7, 0, 0, 9, 0],
    [1, 9, 0, 0, 0, 4, 5, 0, 0],
    [8, 2, 0, 1, 0, 0, 0, 4, 0],
    [0, 0, 4, 6, 0, 2, 9, 0, 0],
    [0, 5, 0, 0, 0, 3, 0, 2, 8],
    [0, 0, 9, 3, 0, 0, 0, 7, 4],
    [0, 4, 0, 0, 5, 0, 0, 3, 6],
    [7, 0, 3, 0, 1, 8, 0, 0, 0]
  ]

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates [] = True
noDuplicates (x : xs) = notElem x xs && noDuplicates xs

validRow :: Row -> Bool
validRow row = noDuplicates $ filter (/= 0) row

validCol board col = validRow $ cols !! col
  where
    cols = Data.List.transpose board

splitChunks :: Int -> [a] -> [[a]]
splitChunks _ [] = []
splitChunks n xs = chunk : splitChunks n rest
  where
    (chunk, rest) = splitAt n xs

boardToBoxes :: Grid -> Grid
boardToBoxes =
  concatMap
    ( \rows ->
        let [r1, r2, r3] = map (splitChunks 3) rows
         in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3
    )
    . splitChunks 3

validBox board box = validRow $ boardToBoxes board !! box

validSquare board col row = validRow (board !! row) && validCol board col && validBox board box
  where
    box = (col `div` 3) * 3 + (row `div` 3)

validBoard :: Grid -> Bool
-- Everything is 0-indexed
validBoard board = and [validSquare board x y | x <- [0 .. 8], y <- [0 .. 8]]

solveBoard :: Grid -> Maybe Grid
solveBoard board = tryNumbers board 0 0 [1 .. 9]

tryNumbers :: Grid -> Int -> Int -> [Int] -> Maybe Grid
tryNumbers _ _ _ [] = Nothing
tryNumbers board row col (n : ns)
  | row >= length board = Just board
  | col >= length (board !! row) = tryNumbers board (row + 1) 0 [1 .. 9]
  | board !! row !! col /= 0 = tryNumbers board row (col + 1) [1 .. 9]
  | validSquare newBoard row col = case tryNumbers newBoard row (col + 1) [1 .. 9] of
      Nothing -> tryNumbers board row col ns
      Just solution -> Just solution
  | otherwise = tryNumbers board row col ns
  where
    newBoard = placeNumber board row col n

placeNumber :: Grid -> Int -> Int -> Int -> Grid
placeNumber board row col n =
  take row board
    ++ [take col (board !! row) ++ [n] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

main :: IO ()
main = do
  print $ solveBoard game1
