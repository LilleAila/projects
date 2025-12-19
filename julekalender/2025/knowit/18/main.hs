{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (group, sort)

splitSections :: (Ord a) => Int -> Int -> [a] -> ([a], [a], [a])
splitSections n d xs = (xs1, xs2, xs3)
  where
    (xs1, r1) = splitAt n xs
    (xs2, r2) = splitAt n . drop d $ r1
    (xs3, _) = splitAt n . drop d $ r2

chunks :: (Ord a) => Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = chunk : chunks n (drop 1 rest)
  where
    (chunk, rest) = splitAt n xs

unzip3' xs = [a, b, c]
  where
    (a, b, c) = unzip3 xs

parseSquare :: Char -> Bool
parseSquare ' ' = False
parseSquare '*' = True

main :: IO ()
main = do
  input <- chunks 10 . map (splitSections 20 3) . lines <$> readFile "input.txt"
  let grids = concatMap ((map . map . map) parseSquare . unzip3') input
      target = head [x | (x : ys) <- group (sort grids), not (null ys)]
      indices = [i + 1 | (i, x) <- zip [0 ..] grids, x == target]
  putStrLn $ concatMap show indices
