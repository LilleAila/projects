-- Similar to v3, but finds the optimal square with the lowest number of
-- possibilities for each iteration of reducing the board.

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Data.Char (digitToInt)
import Data.List (intercalate, transpose, (\\))
import Data.Maybe (fromJust)

-- Types
data Square = Possible [Int] | Fixed Int deriving (Eq, Ord)

instance Show Square where
  show (Fixed square) = show square
  show (Possible possibilities) = "."

type Row = [Square]

type Board = [Row]

-- Utility functions
padRight :: Int -> String -> String
padRight len str
  | length str >= len = str
  | otherwise = str ++ replicate (len - length str) ' '

splitChunks :: (Ord a) => Int -> [a] -> [[a]]
splitChunks _ [] = []
splitChunks n xs = chunk : splitChunks n rest
  where
    (chunk, rest) = splitAt n xs

-- Printing
showPossibilities :: Board -> String
showPossibilities board = intercalate "\n" (map showRow board)
  where
    showRow :: Row -> String
    showRow squares = unwords $ map showSquare squares
    showSquare square = case square of
      Fixed square -> padRight 11 $ show square
      Possible possibilities -> '[' : concatMap (\n -> if n `elem` possibilities then show n else " ") [1 .. 9] ++ "]"

showBoard :: Board -> String
showBoard board = top_hline ++ intercalate middle_hline (map (intercalate "\n") (splitChunks 3 (map showRow board))) ++ bottom_hline
  where
    hline = replicate 3 $ replicate 7 '─'
    vline = "│"
    top_hline = "┌" ++ intercalate "┬" hline ++ "┐\n"
    middle_hline = "\n├" ++ intercalate "┼" hline ++ "┤\n"
    bottom_hline = "\n└" ++ intercalate "┴" hline ++ "┘"

    showRow :: Row -> String
    showRow squares = vline ++ " " ++ intercalate (" " ++ vline ++ " ") (map unwords (splitChunks 3 (map show squares))) ++ " " ++ vline

-- Solver
removeNumbers :: Row -> Maybe Row
removeNumbers squares = traverse pruneSquare squares
  where
    fixed = [x | Fixed x <- squares]
    -- Remove all possibilities that are present in fixed
    pruneSquare (Possible xs) = case xs \\ fixed of
      [] -> Nothing
      [y] -> Just $ Fixed y
      ys -> Just $ Possible ys
    pruneSquare x = Just x

rowsToCols :: Board -> Board
rowsToCols = transpose

boardToBoxes :: Board -> Board
boardToBoxes =
  concatMap
    ( \rows ->
        let [r1, r2, r3] = map (splitChunks 3) rows
         in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3
    )
    . splitChunks 3

-- Repeatedly traverse the board until there are no changes between iterations
traverseBoard :: Board -> Maybe Board
traverseBoard board = traverseBoard' board >>= \x' -> if x' == board then return board else traverseBoard x'

traverseBoard' :: Board -> Maybe Board
traverseBoard' board =
  -- traverse is like map, but wrapped in contexts such as `Maybe`
  traverse removeNumbers board
    -- fmap is like map, but can act on more than just lists
    -- namely, monads such as `Maybe`, which the function returns.
    >>= fmap rowsToCols . traverse removeNumbers . rowsToCols
    -- >>= Is used to chain functions with monadic values (again, such as `Maybe`)
    -- The result from one fmap is passed to the next. If one of them return Nothing, everything returns Nothing.
    >>= fmap boardToBoxes . traverse removeNumbers . boardToBoxes

-- Recursive solver with backtracking (similar to v3 but faster)
setSquare :: Board -> Int -> Int -> Square -> Board
setSquare board row col square =
  take row board
    ++ [take col (board !! row) ++ [square] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

nextBoards :: Board -> (Board, Board)
nextBoards board = (setSquare board row col fixedSquare, setSquare board row col remainderSquare)
  where
    numPossibilities :: Square -> Int
    numPossibilities square = case square of
      Fixed x -> maxBound -- Give fixed values the highest number, because we don't want to select them here.
      Possible xs -> length xs

    -- Find the optimal square (the one with the fewest possibilities)
    (row, col) = foldl1 (\(y2, x2) (y, x) -> if numPossibilities (board !! y !! x) < numPossibilities (board !! y2 !! x2) then (y, x) else (y2, x2)) [(y, x) | y <- [0 .. length board - 1], x <- [0 .. length (head board) - 1]]

    square = board !! row !! col
    fixedSquare = case square of
      Fixed x -> Fixed x
      Possible (x : _) -> Fixed x
    remainderSquare = case square of
      Fixed x -> Fixed x
      Possible [x] -> Fixed x
      Possible (_ : xs) -> Possible xs

isFixed :: Square -> Bool
isFixed square = case square of
  Fixed _ -> True
  Possible _ -> False

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates [] = True
noDuplicates (x : xs) = notElem x xs && noDuplicates xs

validBoard :: Board -> Bool
validBoard board = all validRow board && all validRow (rowsToCols board) && all validRow (boardToBoxes board)
  where
    validRow :: Row -> Bool
    validRow = noDuplicates . filter isFixed

fullBoard :: Board -> Bool
fullBoard = all (all isFixed)

solvedBoard :: Board -> Bool
solvedBoard board = fullBoard board && validBoard board

solveBoard :: Maybe Board -> Maybe Board
solveBoard Nothing = Nothing
solveBoard (Just board)
  | solvedBoard board = Just board
  | otherwise = traverseBoard board >>= solveBoard'
  where
    solveBoard' board
      | not (validBoard board) = Nothing
      | otherwise =
          let (nextBoard, remainderBoard) = nextBoards board -- <|> works with monadic functions, and if the left one returns Nothing, it will instead use the result from the right one, and if both return Nothing, the expression will return Nothing.
           in solveBoard (Just nextBoard) <|> solveBoard (Just remainderBoard)

-- Input
parseBoard :: String -> Board
parseBoard board = map (map parseSquare) $ splitChunks 9 board
  where
    parseSquare :: Char -> Square
    parseSquare square
      | square == '.' = Possible [1 .. 9]
      | otherwise = Fixed $ digitToInt square

board1 = parseBoard "...26.7.168..7..9.19...45..82.1...4...46.29...5...3.28..93...74.4..5..367.3.18..."

board2 = parseBoard "6......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

board3 = parseBoard ".......12.5.4............3.7..6..4....1..........8....92....8.....51.7.......3..."

board4 = parseBoard ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

main :: IO ()
main = do
  putStrLn $ showBoard board4
  putStrLn $ showBoard $ fromJust $ solveBoard (Just board4)
