import Data.List (intercalate, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

data Candy = Candy
  { name :: String,
    price :: Int,
    weight :: Float,
    sugar :: Int,
    maxWeight :: Int,
    minAmount :: Int
  }
  deriving (Show, Eq, Ord)

-- Map from name to number of boxes
type Counts = Map.Map String Int

-- Map from total cost to total suger and the candies used
type Sugar = Map.Map Int (Int, Counts)

budget :: Int
budget = 50000

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn s = foldr f [[]]
  where
    f x acc@(c : cs)
      | x == s = [] : acc
      | otherwise = (x : c) : cs

toCandy :: String -> Candy
toCandy c = Candy name (read price) (read weight) (read sugar) (read max) (read min)
  where
    [name, price, weight, sugar, max, min] = splitOn ',' c

-- Sugar quantity is not needed in the answer.
-- Thus, we can scale this as the ratio will still be the same.
sugarPerCandy :: Candy -> Int
sugarPerCandy c =
  round (weight c * fromIntegral (sugar c) * 10) -- Avoid floats

maxCandies :: Candy -> Int
maxCandies c = floor $ fromIntegral (maxWeight c) / weight c

best :: (Int, Counts) -> (Int, Counts) -> (Int, Counts)
best a b = if fst a >= fst b then a else b

addCandy :: Candy -> Int -> Sugar -> Sugar
addCandy c remaining sugar =
  foldl step sugar (Map.toList sugar)
  where
    p = price c
    s = sugarPerCandy c

    step acc (cost, (sugar, counts)) =
      foldl (tryAdd cost sugar counts) acc [1 .. remaining]

    tryAdd cost sugar counts acc k =
      let newCost = cost + k * p
       in if newCost > budget
            then acc
            else
              let sugar' = sugar + k * s
                  counts' = Map.insertWith (+) (name c) k counts
               in Map.insertWith best newCost (sugar', counts') acc

main :: IO ()
main = do
  candies <- map toCandy . lines <$> readFile "input.txt"

  let baseCounts = Map.fromList [(name c, minAmount c) | c <- candies]
      baseCost = sum [minAmount c * price c | c <- candies]
      baseSugar = sum [minAmount c * sugarPerCandy c | c <- candies]

  let sugar0 :: Sugar
      sugar0 = Map.singleton baseCost (baseSugar, baseCounts)

  let sugar =
        foldl
          ( \sugar c ->
              let remaining = maxCandies c - minAmount c
               in if remaining <= 0 then sugar else addCandy c remaining sugar
          )
          sugar0
          candies

  let (bestCost, (_, bestCounts)) = maximumBy (comparing (fst . snd)) (Map.toList sugar)

  putStrLn $ intercalate "," (show bestCost : [n ++ ":" ++ show (bestCounts Map.! n) | c <- candies, let n = name c])
