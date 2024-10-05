-- This function only works with n <= 10000
-- It should be easy to just add entries for "hunded", "thousand", "million" etc.
-- then use those instead of the two repeated lines with minor differences
numberToWord :: Int -> String
numberToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"
  | n == 20 = "twenty"
  | n == 30 = "thirty"
  | n == 40 = "forty"
  | n == 50 = "fifty"
  | n == 60 = "sixty"
  | n == 70 = "seventy"
  | n == 80 = "eighty"
  | n == 90 = "ninety"
  | n < 100 = numberToWord ((n `div` 10) * 10) ++ " " ++ numberToWord (n `mod` 10)
  | n `mod` 100 == 0 && n < 1000 = numberToWord (n `div` 100) ++ " hundred"
  | n < 1000 = numberToWord ((n `div` 100) * 100) ++ " and " ++ numberToWord (n `mod` 100)
  | n `mod` 1000 == 0 && n < 10000 = numberToWord (n `div` 1000) ++ " thousand"
  | n < 10000 = numberToWord ((n `div` 1000) * 1000) ++ " " ++ numberToWord (n `mod` 1000)

main :: IO ()
main = do
  print $ sum [length $ filter (/= ' ') $ numberToWord x | x <- [1 .. 1000]]
