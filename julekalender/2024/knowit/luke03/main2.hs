import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Name, Amount, Number eaten, Seconds since eaten / seconds since start depending on name
type Food = [(String, Int, Int, Int)]

type Refill = Map.Map String [Int]

foodTypes = ["ris", "erter", "gulrøtter", "reinsdyrkjøtt", "julekringle"]

food :: Food
food = map (,100,0,0) foodTypes

getAmount :: Food -> String -> Int
getAmount [] _ = 0
getAmount ((name, amount, _, _) : food) target
  | name == target = amount
  | otherwise = getAmount food target

foodRefill :: Refill
foodRefill =
  Map.fromList
    [ ("ris", [0, 0, 1, 0, 0, 2]),
      ("erter", [0, 3, 0, 0]),
      ("gulrøtter", [0, 1, 0, 0, 0, 8]),
      ("reinsdyrkjøtt", [100, 80, 40, 20, 10]),
      ("julekringle", [0])
    ]

getRefill :: String -> [Int]
getRefill name = fromMaybe [0] (Map.lookup name foodRefill)

-- Add food
refillFood :: Food -> Food
refillFood [] = []
refillFood x@((name, amount, n, s) : xs)
  | name == "gulrøtter" = if s >= 30 then (name, amount + nextRefill, n + 1, s + 1) : next else noChange : next
  | name == "reinsdyrkjøtt" = if amount <= 0 then if s >= 50 then (name, amount + if n < length refill then nextRefill else 0, n + 1, 0) : next else noChange : next else (name, amount, n, 0) : next
  | name == "julekringle" = noChange : next
  | otherwise = (name, amount + nextRefill, n + 1, s + 1) : next
  where
    refill = getRefill name
    nextRefill = refill !! (n `mod` length refill)
    noChange = (name, amount, n, s + 1)
    next = refillFood xs

-- Eat food
eatFood' :: Int -> Food -> Food
eatFood' _ [] = []
eatFood' eaten x@((name, amount, n, s) : xs)
  | eaten >= 2 = x
  | name == "reinsdyrkjøtt" = if eaten == 0 && amount >= 2 then eat 2 : xs else next
  | name == "julekringle" = if eaten == 0 && amount >= 1 then eat 1 : xs else next
  | amount >= grams = eat grams : eatFood' (eaten + 1) xs
  | otherwise = next
  where
    grams = if eaten == 0 then 5 else 3
    next = (name, amount, n, s) : eatFood' eaten xs
    eat g = (name, amount - g, n, s)

eatFood :: Food -> Food
eatFood = eatFood' 0

refillAndEat :: Food -> Food
refillAndEat = refillFood . eatFood

solve :: Food -> Int
solve food
  | newFood `getAmount` "julekringle" <= 99 = 0
  | otherwise = 1 + solve newFood
  where
    newFood = refillAndEat food

main :: IO ()
main = do
  print $ solve food
  -- print $ take 100 $ iterate refillAndEat food
  print $ iterate refillAndEat food !! 22
