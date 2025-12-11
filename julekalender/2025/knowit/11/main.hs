data Item = Item String Int Int deriving (Show, Eq, Ord)

parseItem :: [String] -> Item
parseItem [name, weight, happiness] = Item name (read weight) (read happiness)

knapsack :: Int -> [Item] -> (Int, [Item])
knapsack _ [] = (0, [])
knapsack max (i@(Item _ weight happiness) : is)
  | weight > max = knapsack max is
  | h2 > h1 = (h2, i : is2)
  | otherwise = (h1, is1)
  where
    (h1, is1) = knapsack max is
    (h2', is2) = knapsack (max - weight) is
    h2 = h2' + happiness

main :: IO ()
main = do
  (weight' : items') <- lines <$> readFile "input.txt"
  let items = map (parseItem . words) items'
  let weight = read weight'
  let (happiness, is) = knapsack weight items
  putStrLn $ show (length is) ++ "," ++ show happiness
