collatz :: Int -> [Int]
collatz 1 = [1] -- Assumes all sequences end at 1
collatz n
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3 * n + 1)

main :: IO ()
main = do
  let sequences = [(length (collatz x), x) | x <- [1 .. 999999]]
  print $ snd $ maximum sequences
