import Data.Char (isDigit)

isConsonant :: Char -> Bool
isConsonant = (`elem` "bcdfghjklmnpqrstvwxz")

bean xs i
  | isDigit x = False
  | isConsonant x = (i + 2 < length xs && i - 2 >= 0) && (isDigit (xs !! (i + 2)) && isDigit (xs !! (i - 2)))
  | otherwise = True
  where
    x = xs !! i

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr $ map (input !!) (filter (bean input) [0 .. length input - 1])
