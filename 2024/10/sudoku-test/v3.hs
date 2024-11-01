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
makeFixed :: Board -> Int -> Int -> Board
makeFixed board row col =
  take row board
    ++ [take col (board !! row) ++ [fixedSquare] ++ drop (col + 1) (board !! row)]
    ++ drop (row + 1) board
  where
    square = board !! row !! col
    fixedSquare = case square of
      Fixed x -> Fixed x
      Possible (y : _) -> Fixed y

makeRemainder :: Board -> Int -> Int -> Maybe Board
makeRemainder board row col =
  case square of
    Fixed _ -> Nothing
    _ -> Just updatedBoard
  where
    square = board !! row !! col
    remainderSquare = case square of
      Fixed x -> Fixed x
      Possible [x] -> Fixed x
      Possible (_ : xs) -> Possible xs

    updatedBoard =
      take row board
        ++ [take col (board !! row) ++ [remainderSquare] ++ drop (col + 1) (board !! row)]
        ++ drop (row + 1) board

isFixed :: Square -> Bool
isFixed square = case square of
  Fixed _ -> True
  Possible _ -> False

solveBoard :: Maybe Board -> Int -> Int -> Maybe Board
solveBoard Nothing _ _ = Nothing
solveBoard (Just board) row col
  | row >= length board = Just board
  | col >= length (board !! row) = solveBoard (Just board) (row + 1) 0
  | isFixed (board !! row !! col) = solveBoard (Just board) row (col + 1)
  | otherwise = case traversedBoard of
      Nothing -> case nextBoard of
        Just nextBoard -> solveBoard (Just nextBoard) row col
        Nothing -> Nothing
      Just traversedBoard -> case solvedBoard of
        Just solvedBoard -> Just solvedBoard
        Nothing -> case nextBoard of
          Just nextBoard -> solveBoard (Just nextBoard) row col
          Nothing -> Nothing
  where
    newBoard = makeFixed board row col
    nextBoard = makeRemainder board row col
    traversedBoard = traverseBoard newBoard
    solvedBoard = solveBoard traversedBoard row (col + 1)

-- Input
parseBoard :: String -> Board
parseBoard board = map (map parseSquare) $ splitChunks 9 board
  where
    parseSquare :: Char -> Square
    parseSquare square
      | square == '.' = Possible [1 .. 9]
      | otherwise = Fixed $ digitToInt square

board1 = parseBoard "...26.7.168..7..9.19...45..82.1...4...46.29...5...3.28..93...74.4..5..367.3.18..."

board2 = parseBoard ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

main :: IO ()
main = do
  -- putStrLn $ showPossibilities board2
  putStrLn $ showBoard board2
  -- putStrLn $ showPossibilities $ fromJust $ traverseBoard board2
  -- putStrLn $ showBoard $ fromJust $ traverseBoard board2
  -- putStrLn $ showPossibilities $ makeFixed board2 0 0
  -- putStrLn $ showPossibilities $ makeRemainder board2 0 0
  -- print $ [show x ++ show y ++ " " ++ show (length $ makeFixed board2 x y) | x <- [0 .. 8], y <- [0 .. 8]]
  -- putStrLn $ showBoard $ makeFixed board2 7 3
  putStrLn $ showBoard $ fromJust $ solveBoard (Just board2) 0 0
