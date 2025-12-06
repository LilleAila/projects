-- Rather than trying every single number for each square, this implementation
-- will traverse the board, removing all numbers that are present in intersecting
-- fixed squares. This will lead to a smaller amount of possibilities to check
-- It does this recursively, testing all possibilities until the board is solved
-- Possible improvements include:
-- Doing a breadth-first search rather than a depth-first search
-- Finding the optimal next square rather than incrementing by one for the next boards
-- Using a data structure such as a vector, instead of a list

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

-- Recursive solver with backtracking (similar to v1 but way faster)
setSquare :: Board -> Int -> Int -> Square -> Board
setSquare board row col square =
  take row board
    ++ [take col (board !! row) ++ [square] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board

makeFixed :: Board -> Int -> Int -> Board
makeFixed board row col = setSquare board row col fixedSquare
  where
    square = board !! row !! col
    fixedSquare = case square of
      Fixed x -> Fixed x
      Possible (y : _) -> Fixed y

makeRemainder :: Board -> Int -> Int -> Maybe Board
makeRemainder board row col = case square of
  Fixed _ -> Nothing
  _ -> Just $ setSquare board row col remainderSquare
  where
    square = board !! row !! col
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

solveBoard :: Maybe Board -> Int -> Int -> Maybe Board
solveBoard Nothing _ _ = Nothing
solveBoard (Just board) row col
  | row >= length board = Just board
  | col >= length (board !! row) = solveBoard (Just board) (row + 1) 0
  | isFixed (board !! row !! col) = solveBoard (Just board) row (col + 1)
  | otherwise = traverseBoard board >>= solveBoard'
  where
    solveBoard' board
      | not (validBoard board) = Nothing
      | otherwise =
          let nextBoard = makeFixed board row col
              remainderBoard = makeRemainder board row col
           in -- <|> works with monadic functions, and if the left one returns Nothing, it will instead use the result from the right one, and if both return Nothing, the expression will return Nothing.
              solveBoard (Just nextBoard) row (col + 1) <|> solveBoard remainderBoard row col

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

main :: IO ()
main = do
  putStrLn $ showBoard board1
  putStrLn $ showBoard $ fromJust $ solveBoard (Just board2) 0 0
