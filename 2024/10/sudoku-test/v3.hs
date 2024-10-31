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

-- Input
parseBoard :: String -> Board
parseBoard board = map (map parseSquare) $ splitChunks 9 board
  where
    parseSquare :: Char -> Square
    parseSquare square
      | square == '.' = Possible [1 .. 9]
      | otherwise = Fixed $ digitToInt square

board1 = parseBoard "...26.7.168..7..9.19...45..82.1...4...46.29...5...3.28..93...74.4..5..367.3.18..."

main :: IO ()
main = do
  putStrLn $ showPossibilities board1
  putStrLn $ showBoard board1
  putStrLn $ showBoard $ fromJust $ traverseBoard board1
