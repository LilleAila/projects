import Data.Char (ord)
import Data.List (sort)

-- Downloaded the file from https://projecteuler.net/resources/documents/0022_names.txt
-- Formatted it in vim to separate with newlines, remove quotes and set to lowercase

alphabetIndex :: Char -> Int
alphabetIndex x = ord x - ord 'a' + 1

main :: IO ()
main = do
  names' <- readFile "p022.txt"
  let names = sort $ lines names'
  let nameScores = [sum (map alphabetIndex y) * x | (x, y) <- zip [1 ..] names]
  print $ sum nameScores
