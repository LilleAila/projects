{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (tails)

substr :: String -> Int -> Int -> Int -> String -> Bool
substr [] _ _ _ _ = True
substr _ _ _ _ [] = False
substr ts@(t : ts') min max n (x : xs)
  | n > max = False
  | x == t && n >= min = substr ts' min max 0 xs || next
  | otherwise = next
  where
    next = substr ts min max (n + 1) xs

substrAll :: (String -> Bool) -> String -> Bool
substrAll f = any f . tails

troll :: String -> Bool
troll xs
  | length xs < 9 = False
  | otherwise = substrAll (substr "troll" 1 5 5) xs

nisse :: String -> Bool
nisse xs
  | length xs <= 5 = False
  | otherwise = substrAll (substr "nisse" 0 2 2) xs && head xs /= 'n' && last xs /= 'e'

assert :: Bool -> String -> IO ()
assert True _ = return ()
assert False msg = putStrLn $ "Assertion failed: " ++ msg

main :: IO ()
main = do
  words <- lines <$> readFile "input.txt"
  print $ length [x | x <- words, troll x || nisse x]
