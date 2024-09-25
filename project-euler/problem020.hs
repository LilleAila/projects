import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x - 1)

-- factorial x = product [1 .. x]

main :: IO ()
main = do
  print $ sum $ map digitToInt $ show $ factorial 100
