-- We have given the following formula:
-- S = x (2a + b) + y c
-- This has two unknowns, we can solve it using two given values:
-- 4445 = x * (2*91 - 12) + y * 13
-- 3855 = x * (2*87 - 24) + y * 7
-- This gives x=25 and y=15

import Data.List (intercalate, sortOn)

parseLine l = (n, score)
  where
    [n, sn, sl, p] = words l
    score = 25 * (2 * read sn - read sl) + 15 * read p

solve ls = map (\(n, s) -> n ++ " " ++ show s) scores
  where
    scores' = (sortOn snd . map parseLine) ls
    scores = (take 3 . reverse) scores' ++ (reverse . take 3) scores'

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ (intercalate "," . solve . lines) content
