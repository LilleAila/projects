import Data.Char (digitToInt)
import Data.List (intercalate)

data Square = Possible [Int] | Fixed Int

instance Show Square where
  show (Fixed square) = show square
  show (Possible possibilities) = "."

type Row = [Square]

type Board = [Row]

padRight :: Int -> String -> String
padRight len str
  | length str >= len = str
  | otherwise = str ++ replicate (len - length str) ' '

splitChunks :: (Ord a) => Int -> [a] -> [[a]]
splitChunks _ [] = []
splitChunks n xs = chunk : splitChunks n rest
  where
    (chunk, rest) = splitAt n xs

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
