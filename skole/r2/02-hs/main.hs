import Data.List (intercalate)

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

fibonacci' :: [Integer]
fibonacci' = 1 : 1 : zipWith (+) fibonacci' (drop 1 fibonacci')

main :: IO ()
main = do
  -- print $ take 20 $ map fibonacci [1 ..]
  -- print $ take 20 fibonacci'
  let fibs = take 500 fibonacci'
  let results = zip [1 ..] fibs
  writeFile "fibonacci.dat" $ unlines [show x ++ " " ++ show y | (x, y) <- results]
