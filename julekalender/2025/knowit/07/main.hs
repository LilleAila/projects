{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (tails)

substr :: String -> Int -> Int -> Int -> Bool -> String -> Bool
substr [] _ _ _ _ _ = True
substr _ _ _ _ _ [] = False
substr ts@(t : ts') min max n start (x : xs)
  | n > max && not start = False -- Allow unlimited chars before first target char
  | x == t && n >= min = substr ts' min max 0 False xs || next
  | otherwise = next
  where
    next = substr ts min max (n + 1) start xs

troll :: String -> Bool
troll xs
  | length xs < 9 = False -- Early return if too short (has a very slight performance benefit)
  | otherwise = substr "troll" 1 5 0 True xs

nisse :: String -> Bool
nisse xs
  | length xs <= 5 = False
  | otherwise = substr "nisse" 0 2 0 True xs && head xs /= 'n' && last xs /= 'e'

main :: IO ()
main = do
  words <- lines <$> readFile "input.txt"
  print $ length [x | x <- words, troll x || nisse x]
