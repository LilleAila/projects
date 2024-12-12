module Main where

import Data.Map qualified as Map

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

-- NOTE: The reach only goes upwards in the file, so there is no risk of loops from going down.
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

distance :: Coordinate -> Coordinate -> Double
distance (y1, x1) (y2, x2) = sqrt $ fromIntegral ((y2 - y1) ^ 2 + (x2 - x1) ^ 2)

makeGraph :: Reach -> [Grip] -> Map.Map Grip [Grip]
makeGraph reach grips = Map.fromList [(grip, nextGrips grip) | grip <- grips]
  where
    nextGrips grip = filter (isWithinReach reach grip) grips

main :: IO ()
main = do
  gripsInput <- readFile "../grep_short.txt" -- Only first 10 Y
  reachInput <- readFile "../reach.txt"
  let grips = parseGrips gripsInput
  let reach = parseReach reachInput
  let graph = makeGraph reach grips
  print graph
