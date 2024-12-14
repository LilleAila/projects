data Match = Match
  { team :: String,
    time :: Int,
    score :: Int,
    opposingScore :: Int,
    assists :: Int,
    goals :: Int,
    best :: Bool
  }
  deriving (Show, Eq)

teams = [("FCB", 1), ("J", 2), ("NT", 1), ("NF", 1), ("RC", 3), ("SP", 3), ("VM", 3)]

getTeam :: String -> Int
getTeam x = (snd . head) (filter ((== x) . fst) teams)

parseInput = map parseLine . lines
  where
    parseLine line =
      Match
        { team = team,
          time = read time,
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

main :: IO ()
main = do
  input <- readFile "salary'.txt"
  let matches = parseInput input
  print matches
