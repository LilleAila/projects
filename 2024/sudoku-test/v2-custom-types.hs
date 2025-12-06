-- Creating types with data and newtype
-- Makes development harder, so using external functions to print the board instead

import Data.Char (digitToInt)
import Data.List (intercalate)

splitChunks :: (Ord a) => Int -> [a] -> [[a]]
splitChunks _ [] = []
splitChunks n xs = chunk : splitChunks n rest
  where
    (chunk, rest) = splitAt n xs

padRight :: Int -> String -> String
padRight len str
  | length str >= len = str
  | otherwise = str ++ replicate (len - length str) ' '

data Square = Possible [Int] | Fixed Int

instance Show Square where
  show (Fixed square) = padRight 11 $ show square
  show (Possible possibilities) = '[' : concatMap (\n -> if n `elem` possibilities then show n else " ") [1 .. 9] ++ "]"

newtype Row = Row [Square]

instance Show Row where
  show (Row squares) = unwords $ map show squares

newtype Board = Board [Row]

instance Show Board where
  show (Board board) = intercalate "\n" (map show board)

parseBoard :: String -> Board
parseBoard board = Board . map (Row . map parseSquare) $ splitChunks 9 board
  where
    parseSquare :: Char -> Square
    parseSquare square
      | square == '.' = Possible [1 .. 9]
      | otherwise = Fixed $ digitToInt square

board1 = parseBoard "...26.7.168..7..9.19...45..82.1...4...46.29...5...3.28..93...74.4..5..367.3.18..."

main :: IO ()
main = do
  print board1
