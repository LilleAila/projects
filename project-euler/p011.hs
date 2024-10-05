import Data.Bits (Bits (xor))
import Data.List
import System.IO

productsHorizontal :: [Int] -> [Int]
productsHorizontal x
  | length x < 4 = []
  | otherwise = product (take 4 x) : productsHorizontal (tail x)

productsDiagonal :: [[Int]] -> [Int]
productsDiagonal xs = [diagonalProduct y x | x <- [0 .. len_x - 3], y <- [0 .. len_y - 3]]
  where
    len_y = length xs - 1
    len_x = length (head xs) - 1
    diagonalProduct y x = (xs !! y !! x) * (xs !! (y + 1) !! (x + 1)) * (xs !! (y + 2) !! (x + 2)) * (xs !! (y + 3) !! (x + 3))

productsDiagonal' :: [[Int]] -> [Int]
productsDiagonal' xs = [diagonalProduct y x | x <- [3 .. len_x], y <- [0 .. len_y - 3]]
  where
    len_y = length xs - 1
    len_x = length (head xs) - 1
    diagonalProduct y x = (xs !! y !! x) * (xs !! (y + 1) !! (x - 1)) * (xs !! (y + 2) !! (x - 2)) * (xs !! (y + 3) !! (x - 3))

main :: IO ()
main = do
  grid <- readFile "problem011.txt"
  let numbers = [map (\x -> read x :: Int) $ words x | x <- lines grid]

  let horizontal = concatMap productsHorizontal numbers
  let vertical = concatMap productsHorizontal $ transpose numbers -- transpose rotates a matrix
  let diagonal = productsDiagonal numbers
  let diagonal' = productsDiagonal' numbers

  print $ maximum $ horizontal ++ vertical ++ diagonal ++ diagonal'
