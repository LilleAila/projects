isPalindrome x = x == reverseInt x 0
  where
    reverseInt :: Int -> Int -> Int
    -- start at the end, and push by 10 to the next digit until 0
    reverseInt 0 y = y
    -- div is integer division
    reverseInt x y = reverseInt (x `div` 10) (y * 10 + x `mod` 10)

threeDigitPalindromes = filter isPalindrome products
  where
    products = [x * y | x <- [100 .. 999], y <- [100 .. 999]]

main :: IO ()
main = do
  print $ maximum threeDigitPalindromes
