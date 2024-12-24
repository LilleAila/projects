import Data.List (nub, permutations)

vampireNumber :: Int -> (Int, Int) -> Bool
vampireNumber n (x, y) = n == x * y

fangs n = fangPermutations
  where
    digits = show n
    fangPermutations' = nub $ permutations digits
    fangPermutations =
      [ (read a, read b)
        | p <- fangPermutations',
          i <- [1 .. length digits - 1],
          let (a, b) = splitAt i p,
          not (null a || null b),
          head a /= '0',
          head b /= '0'
      ]

pseudoVampireNumber n = any (vampireNumber n) (fangs n)

main :: IO ()
main = do
  input <- readFile "tall.txt"
  let numbers :: [Int]
      numbers = (map read . lines) input
  print $ (sum . filter pseudoVampireNumber) numbers
