-- Same as Main2, but gets the total distance

module Main where

import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.PSQueue qualified as PSQ
import Data.Set qualified as Set

-- NOTE: coordinates are (Y, X)
type Coordinate = (Int, Int)

type Grip = Coordinate

type Graph = Map.Map Grip [Grip]

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
    parseChar 'o' = False
    parseChar _ = True

isWithinReach :: Reach -> Coordinate -> Grip -> Bool
isWithinReach reach (y, x) (gripY, gripX)
  | row < 0 || row >= length reach || col < 0 || col >= length (head reach) = False
  | otherwise = reach !! row !! col
  where
    relativeX = gripX - x
    relativeY = gripY - y
    row = length reach - relativeY - 1
    col = relativeX + div (length (head reach)) 2 -- Center column calculation

makeGraph :: Reach -> [Grip] -> Graph
makeGraph reach grips = Map.fromList [(grip, nextGrips grip) | grip <- grips]
  where
    nextGrips grip = filter (isWithinReach reach grip) grips

euclideanDistance :: Grip -> Grip -> Double
euclideanDistance (x1, y1) (x2, y2) =
  sqrt (fromIntegral ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int)))

-- NOTE: Algorithm not implemented by me
-- https://en.wikipedia.org/wiki/A*_search_algorithm
aStarDistance :: Graph -> Grip -> Grip -> Maybe Double
aStarDistance graph start goal = go initialQueue Set.empty
  where
    -- Priority queue with (grip, priority)
    initialQueue = PSQ.singleton start 0
    -- A* main loop
    go queue visited
      | PSQ.null queue = Nothing -- No path found
      | current == goal = Just gScoreCurrent
      | current `Set.member` visited = go restQueue visited
      | otherwise =
          let neighbors = Map.findWithDefault [] current graph
              unvisitedNeighbors = filter (`Set.notMember` visited) neighbors
              updatedQueue = foldl' updateQueue restQueue unvisitedNeighbors
           in go updatedQueue (Set.insert current visited)
      where
        -- Current node being explored
        (current PSQ.:-> gScoreCurrent) = fromJust (PSQ.findMin queue)
        restQueue = PSQ.deleteMin queue
        -- Update queue with a neighbor
        updateQueue q neighbor =
          let newCost = gScoreCurrent + euclideanDistance current neighbor
              fScore = newCost + euclideanDistance neighbor goal
           in PSQ.insertWith min neighbor fScore q

main :: IO ()
main = do
  gripsInput <- readFile "../grep.txt" -- Only first 10 Y
  reachInput <- readFile "../reach.txt"
  let grips = parseGrips gripsInput
      reach = parseReach reachInput
      graph = makeGraph reach grips
      start = (0, 250)
      -- target = (12, 259)
      target = (999, 749)
  print $ floor . (* 10) <$> aStarDistance graph start target
