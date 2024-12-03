import qualified Data.Map as Map

-- Name, Amount, Number eaten, Seconds since eaten
type Food = [(String, Int, Int, Int)]

type Refill = Map.Map String [Int]

foodTypes = ["ris", "erter", "gulrøtter", "reinsdyrkjøtt", "julekringle"]

food :: Food
food = map (,100,0,0) foodTypes

foodRefill :: Refill
foodRefill =
  Map.fromList
    [ ("ris", [0, 0, 1, 0, 0, 2]),
      ("erter", [0, 3, 0, 0]),
      ("gulrøtter", [0, 1, 0, 0, 0, 8]),
      ("reinsdyrkjøtt", [100, 80, 40, 20, 10]),
      ("julekringle", [0])
    ]

getFood :: Food -> String -> Int
getFood [] _ = 0
getFood ((name, amount, _, _) : food) target
  | name == target = amount
  | otherwise = getFood food target

refillFood :: Food -> Refill -> Int -> Food
refillFood [] _ _ = []
refillFood food@((name, amount, n, s) : xs) refill seconds
  | name == "gulrøtter" = if seconds >= 30 then (name, amount + refillAmount, n + 1, s + 1) : refillFood xs refill seconds else (name, amount, n, s + 1) : refillFood xs refill seconds
  | name == "reinsdyrkjøtt" = if amount <= 0 && s >= 50 && n < 5 then (name, amount + refillAmount, n + 1, 0) : refillFood xs refill seconds else (name, amount, n, s + 1) : refillFood xs refill seconds
  | name == "julekringle" = (name, amount, n, s + 1) : refillFood xs refill seconds
  | otherwise = (name, amount + refillAmount, n + 1, s + 1) : refillFood xs refill seconds
  where
    refillAmount = case Map.lookup name refill of
      Just x -> x !! (n `mod` length x)
      Nothing -> 0

eatTwo' :: [Int] -> Int -> Food -> Food
eatTwo' _ _ [] = []
eatTwo' [] _ x = x
eatTwo' g@(gram : grams) eaten x@((name, amount, n, s) : xs)
  | eaten >= 2 = x
  | amount > 0 = (name, max 0 (amount - gram), n, s) : eatTwo' grams (eaten + 1) xs
  | otherwise = (name, amount, n, s) : eatTwo' g eaten xs

eatTwo :: Food -> Food
eatTwo = eatTwo' [5, 3] 0

eatAndRefill :: Food -> Refill -> Int -> Food
eatAndRefill food = refillFood (eatTwo food)

solve :: Food -> Refill -> Int -> Int
solve food refill seconds
  | newFood `getFood` "julekringle" <= 99 = seconds + 1
  | otherwise = solve newFood refill (seconds + 1)
  where
    newFood = eatAndRefill food refill seconds

main :: IO ()
main = do
  -- let test = iterate eatTwo food !! 40
  -- print food
  -- print test
  -- print $ refillFood food foodRefill 0
  -- let answer = solve food foodRefill 0
  -- print answer
  print food
  print $ eatAndRefill food foodRefill 0
  print $ eatAndRefill (eatAndRefill food foodRefill 0) foodRefill 1
  print $ eatAndRefill (eatAndRefill (eatAndRefill food foodRefill 0) foodRefill 1) foodRefill 2
