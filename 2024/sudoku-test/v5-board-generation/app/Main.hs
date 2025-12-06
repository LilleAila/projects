-- Management of "random" numbers:
-- Parent randomGen with seed 1
-- For each board, use the head of parent randoms as the seed for a child random
-- Repeat for the next board, using the tail of the parent randomGen.
-- This ensures reproducability while using random numbers.

module Main (main) where

import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.Random (mkStdGen, randoms)
import System.Random.Shuffle (shuffle')

-- Types and output
type Square = Int

type Row = [Square]

type Board = [Row]

showBoard :: Board -> String
showBoard board = top_hline ++ intercalate middle_hline (map (intercalate "\n") (chunksOf 3 (map showRow board))) ++ bottom_hline
  where
    hline = replicate 3 $ replicate 7 '─'
    vline = "│"
    top_hline = "┌" ++ intercalate "┬" hline ++ "┐\n"
    middle_hline = "\n├" ++ intercalate "┼" hline ++ "┤\n"
    bottom_hline = "\n└" ++ intercalate "┴" hline ++ "┘"

    showRow :: Row -> String
    showRow squares = vline ++ " " ++ intercalate (" " ++ vline ++ " ") (map unwords (chunksOf 3 (map showSquare squares))) ++ " " ++ vline

    showSquare :: Square -> String
    showSquare square = if square == 0 then "." else show square

-- Utility functions for randomness and board validation
-- getRandoms :: Int -> (Int, Int) -> [Int]
-- getRandoms seed range = randomRs range (mkStdGen seed)

getRandoms :: Int -> [Int]
getRandoms = randoms . mkStdGen

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates [] = True
noDuplicates (x : xs) = notElem x xs && noDuplicates xs

validRow :: Row -> Bool
validRow row = noDuplicates $ filter (/= 0) row

colsToRows :: Board -> Board
colsToRows = transpose

boardToBoxes :: Board -> Board
boardToBoxes =
  concatMap
    ( \rows ->
        case map (chunksOf 3) rows of
          [r1, r2, r3] -> zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3
          _ -> error "Invaid board!"
    )
    . chunksOf 3

validBoard :: Board -> Bool
validBoard board = all validRow board && all validRow (colsToRows board) && all validRow (boardToBoxes board)

-- Board generation
setSquare :: Board -> Int -> Int -> Square -> Board
setSquare board row col square =
  take row board
    ++ [take col (board !! row) ++ [square] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

generateBoard' :: Board -> Int -> Int -> [Int] -> [Int] -> Maybe Board
generateBoard' _ _ _ [] _ = Nothing
generateBoard' board row col (n : ns) (r : rs)
  | row >= length board = Just board
  | col >= length (board !! row) = generateBoard' board (row + 1) 0 shuffledNums rs
  | validBoard newBoard = case generateBoard' newBoard row (col + 1) shuffledNums rs of
      Nothing -> generateBoard' board row col ns rs
      Just solution -> Just solution
  | otherwise = generateBoard' board row col ns rs
  where
    newBoard = setSquare board row col n
    shuffledNums = shuffle' [1 .. 9] 9 (mkStdGen r)
generateBoard' _ _ _ _ _ = Nothing

-- IO actions
board1 :: Board
board1 =
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

emptyBoard :: Board
emptyBoard = replicate 9 $ replicate 9 0

main :: IO ()
main = do
  putStrLn $ showBoard board1
  print $ validBoard board1
  print $ take 10 $ getRandoms 1
  putStrLn $ showBoard $ fromJust $ generateBoard' emptyBoard 0 0 [1 .. 9] (getRandoms 1)
