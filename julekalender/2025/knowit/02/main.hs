import Data.Char (isLower, toLower, toUpper)
import Data.List (concat, elemIndex)

alphabet = "abcdefghijklmnopqrstuvwxyzæøå"

shift n x = if isLower x then shift' n x else (toUpper . shift' n . toLower) x
  where
    shift' n x = case elemIndex x alphabet of
      Nothing -> error "wtf"
      Just i -> alphabet !! ((i + n) `mod` length alphabet)

solve = concat . zipWith (map . shift) [-1, -2 ..] . lines

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ solve content
