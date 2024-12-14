count :: (Eq a) => [a] -> a -> Int
count xs y = (length . filter (== y)) xs

countInt :: Int -> Int -> Int
countInt xs y = count (show xs) ((head . show) y)

reduceNumbers :: [Int] -> [Int] -> [Int]
reduceNumbers numbers [] = numbers
reduceNumbers numbers (x : xs) = reduceNumbers newNumbers xs
  where
    -- This is made slightly more complicated because i the lists 0-indexed
    -- , while the task asks for 9 to 0 instead of 0 to 9
    digitCount = sum (map (`countInt` x) (drop (x + 1) numbers))
    newNumbers = take x numbers ++ [(numbers !! x) - digitCount] ++ drop (x + 1) numbers

printNumbers :: [Int] -> [Int] -> Int -> (Int, [Int])
printNumbers numbers empty rows
  | length empty >= length numbers = (rows, empty)
  | otherwise = printNumbers newNumbers newEmpty (rows + 1)
  where
    newNumbers = reduceNumbers numbers [9, 8 .. 0]
    newEmpty = empty ++ filter (< 0) (zipWith (*) numbers newNumbers)

main :: IO ()
main = do
  let numbers = replicate 10 100_000
  -- print $ reduceNumbers numbers [9, 8 .. 0]
  print $ take 10 $ iterate (`reduceNumbers` [9, 8 .. 0]) numbers

-- print $ printNumbers numbers [] 0
