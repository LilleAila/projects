-- NOTE: coordinates are (Y, X)
type Coordinate = (Int, Int)

type Grip = Coordinate

type Reach = [[Bool]]

parseGrips :: String -> [Grip]
parseGrips = map parseGrip . lines
  where
    parseGrip :: String -> Grip
    parseGrip line = case map read (words line) of
      [x, y] -> (x, y)
      _ -> error ("Could not parse grip " ++ line)

parseReach :: String -> Reach
parseReach = map parseLine . lines
  where
    parseLine l = take 19 $ map parseChar l ++ repeat False
    parseChar ' ' = False
    parseChar 'o' = False -- Otherwise creates an infinite loop
    parseChar _ = True

isWithinReach :: Reach -> Coordinate -> Grip -> Bool
isWithinReach reach (y, x) (gripY, gripX)
  | row < 0 || row >= length reach || col < 0 || col >= length (head reach) = False
  | otherwise = reach !! row !! col
  where
    relativeX = gripX - x
    relativeY = gripY - y
    row = length reach - relativeY - 1
    col = relativeX + 9 -- Center it on the "o" in the input

nextGrips :: Reach -> Coordinate -> [Grip] -> [Grip]
nextGrips reach pos = filter (isWithinReach reach pos)

distance :: Coordinate -> Coordinate -> Double
distance (y1, x1) (y2, x2) = sqrt $ fromIntegral ((y2 - y1) ^ 2 + (x2 - x1) ^ 2)

solve :: Reach -> Coordinate -> [Grip] -> Double
solve reach pos@(y, x) grips
  | y == 999 && x == 749 = 0 -- Target
  | null next = 1e9 -- Very high number
  | otherwise = minimum $ map (\grip -> distance pos grip + solve reach grip grips) next
  where
    next = nextGrips reach pos grips

main :: IO ()
main = do
  gripsInput <- readFile "grep.txt"
  reachInput <- readFile "reach.txt"
  let grips = parseGrips gripsInput
  let reach = parseReach reachInput
  let solution = solve reach (0, 250) grips
  (print . floor . (* 10)) solution
