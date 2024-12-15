import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

data Match = Match
  { team :: String,
    minutes :: Int,
    score :: Int,
    opposingScore :: Int,
    assists :: Int,
    goals :: Int,
    best :: Bool
  }
  deriving (Show, Eq)

teams :: Map.Map String Int
teams = Map.fromList [("FCB", 1), ("J", 2), ("NT", 1), ("NF", 1), ("RC", 3), ("SP", 2), ("VM", 3)]

getTeam :: String -> Int
getTeam name = fromMaybe 0 (Map.lookup name teams)

parseInput :: String -> [Match]
parseInput = map parseLine . lines
  where
    parseLine line =
      Match
        { team = team,
          minutes = read time,
          score = if location == "H" then read h else read b,
          opposingScore = if location == "H" then read b else read h,
          assists = count 'A' stats,
          goals = count 'S' stats,
          best = 'B' `elem` stats
        }
      where
        team : time : location : h : b : xs = words line
        stats = if null xs then [] else head xs
        count x = length . filter (== x)

result match losses = fromIntegral (base * multiplier) `div` 100
  where

    base = 100 * minutes match
    p_goals = 5 * score match - 5 * opposingScore match
    p_stats = assists match + 2 * goals match + if best match then 2 else 0
    -- Losses are simply passed as a parameter here, to be calculated externally
    p_losses = if losses > 0 then 3 + losses - 1 else 0 -- Assuming first loss 3 then 1 for each
    p_team
      | score match > opposingScore match = getTeam (team match)
      | score match < opposingScore match = - getTeam (team match)
      | otherwise = 0
    multiplier = 100 + p_goals + p_stats + p_losses + p_team

solve :: [Match] -> Int -> Int
solve [] _ = 0
solve (match:matches) losses = p + solve matches newLosses
  where
    p = result match losses
    newLosses
      | score match < opposingScore match = losses + 1
      | score match > opposingScore match = 0
      | otherwise = losses

main :: IO ()
main = do
  input <- readFile "salary'.txt"
  let matches = parseInput input
  -- print $ result ((head . parseInput) "J 65 B 2 1 AB") 0
  print $ solve matches 0
