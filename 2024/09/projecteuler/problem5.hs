evenlyDivisible :: Int -> [Int] -> Bool
evenlyDivisible divisor = all (\x -> divisor `mod` x == 0) -- Use currying to avoid passing dividends directly

main :: IO ()
main = do
  -- Increment by 20 because it has to be divisible anyways, so there's no point checking
  print $ head [x | x <- [20, 40 ..], evenlyDivisible x [1 .. 20]]
