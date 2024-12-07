powersOf10 :: [Int]
powersOf10 = scanl (*) 1 (repeat 10)

getDigit :: Int -> Int -> Int
getDigit num n
  | n <= 0 = 0
  | otherwise = (abs num `div` (powersOf10 !! (n - 1))) `mod` 10

valid :: Int -> Bool
valid x = ac_ib && bd_eg && gf_de && ba_bb
  where
    digit = getDigit x
    ac_ib = (digit 1 `elem` [0, 2, 3, 5, 6, 7, 8, 9] && digit 9 `elem` [0, 1, 2, 3, 4, 7, 8, 9]) || (digit 1 == 2 && digit 9 `elem` [5, 6])
    bd_eg = (digit 2 `elem` [0, 2, 3, 5, 6, 8, 9] && digit 5 `elem` [2, 3, 4, 5, 6, 8, 9]) || (digit 2 `elem` [1, 4, 7] && digit 5 `elem` [0, 1, 7])
    gf_de = (digit 7 `elem` [0, 4, 5, 6, 8, 9] && digit 4 `elem` [0, 2, 6, 8]) || (digit 7 `elem` [1, 2, 3, 7] && digit 4 `elem` [1, 3, 4, 5, 7])
    ba_bb = digit 2 `elem` [0, 2, 3, 7, 8, 9]

main :: IO ()
main = do
  -- let numbers = [x | x <- [0 .. 999999999], valid x]
  let numbers = filter valid [0 .. 999999999]
  print $ length numbers
