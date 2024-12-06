import Data.Function (on)
import Data.List (groupBy, sortBy, sortOn)
import Data.Ord (comparing)
import System.IO (readFile)

type Candidate = String

type Vote = Int

type CandidateVote = (Candidate, Vote)

type Representative = Int

type State = (Representative, [CandidateVote])

type VotedState = (Candidate, Representative)

parseCandidates :: String -> [Candidate]
parseCandidates = lines

parseStates :: String -> [Candidate] -> [State]
parseStates input candidates = votes
  where
    states = (map words . lines) input
    votes = [(read (head x), votes) | x <- states, let votes = zip candidates ((map read . tail) x)]

diff :: (Num a) => a -> a -> a
a `diff` b = abs $ a - b

addVotes :: State -> [VotedState]
addVotes (representatives, votes) = finalVotes
  where
    totalVotes = sum (map snd votes)

    initialVotes =
      [ (candidate, floor allocated, allocated - fromIntegral (floor allocated))
        | (candidate, v) <- votes,
          let allocated = (fromIntegral v / fromIntegral totalVotes) * fromIntegral representatives
      ]

    rests = sortBy (flip (comparing (\(_, _, rest) -> rest)) <> comparing (\(name, _, _) -> name)) initialVotes

    totalAssigned = sum [floorReps | (_, floorReps, _) <- initialVotes]
    rest = representatives - totalAssigned

    extraRepresentatives = take rest [name | (name, _, _) <- rests]

    finalVotes =
      [ (candidate, floorReps + extra)
        | (candidate, floorReps, _) <- initialVotes,
          let extra = if candidate `elem` extraRepresentatives then 1 else 0
      ]

totalVotes :: [State] -> [VotedState]
totalVotes states = map sumVotes groupedVotes
  where
    votedStates = concatMap addVotes states
    groupedVotes = groupBy ((==) `on` fst) $ sortOn fst votedStates
    sumVotes group = (fst $ head group, sum $ map snd group)

main :: IO ()
main = do
  kandidater <- readFile "kandidater'.txt"
  stater <- readFile "stater'.txt"
  let candidates = parseCandidates kandidater
      votes = parseStates stater candidates
  print candidates
  print $ head votes
  print $ addVotes (head votes)
  print $ totalVotes votes
