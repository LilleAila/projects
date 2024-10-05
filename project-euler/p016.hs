splitInt :: Integer -> [Integer]
splitInt x = reverse $ splitInteger' x
  where
    splitInteger' x
      | x < 10 = [x]
      -- Reversing is more performant than concatenation
      | otherwise = (x `mod` 10) : splitInteger' (x `div` 10)

sumDigits x = sum $ splitInt x

-- main :: IO ()
-- main = do
--   print $ sumDigits (2 ^ 1000)

-- alternatively, if i can convert to strings
{-
import Data.Char (digitToInt)

main :: IO ()
main = do
  print $ sum $ map digitToInt $ show (2 ^ 1000)
-}
