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
  | name == "reinsdyrkjøtt" = if amount <= 0 && s > 50 && n < 5 then (name, amount + refillAmount, n + 1, 0) : refillFood xs refill seconds else (name, amount, n, s + 1) : refillFood xs refill seconds
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
  | eaten >= 2 = (name, amount, n, s + 1) : eatTwo' g eaten xs
  | name == "reinsdyrkjøtt" = if amount >= 2 && eaten == 0 then (name, max 0 (amount - 2), n, s) : eatTwo' grams (eaten + 2) xs else next
  | name == "julekringle" = if amount >= 1 && eaten == 0 then (name, max 0 (amount - 1), n, s) : eatTwo' grams (eaten + 2) xs else next
  | amount >= gram = (name, max 0 (amount - gram), n, s) : eatTwo' grams (eaten + 1) xs
  | otherwise = next
  where
    next = (name, amount, n, s) : eatTwo' g eaten xs

eatTwo :: Food -> Food
eatTwo = eatTwo' [5, 3] 0

eatAndRefill :: Food -> Refill -> Int -> Food
eatAndRefill food = refillFood (eatTwo food)

eatAndRefill' food = eatAndRefill food foodRefill 0

refillAndEat food refill seconds = eatTwo (refillFood food refill seconds)

solve :: Food -> Refill -> Int -> Int
solve food refill seconds
  | newFood `getFood` "julekringle" <= 99 = seconds + 1
  | otherwise = solve newFood refill (seconds + 1)
  where
    newFood = refillAndEat food refill seconds

main :: IO ()
main = do
  -- let test = iterate eatTwo food !! 40
  -- print food
  -- print test
  -- print $ refillFood food foodRefill 0
  print food
  -- print $ eatAndRefill food foodRefill 0
  -- print $ eatAndRefill (eatAndRefill food foodRefill 0) foodRefill 1
  -- print $ eatAndRefill (eatAndRefill (eatAndRefill food foodRefill 0) foodRefill 1) foodRefill 2
  print $ eatAndRefill' food
  print $ iterate eatAndRefill' food !! 22
  print $ iterate eatAndRefill' food !! 23
  print $ iterate eatAndRefill' food !! 24
  -- print $ take 70 $ iterate eatAndRefill' food
  let answer = solve food foodRefill 0
  print answer
