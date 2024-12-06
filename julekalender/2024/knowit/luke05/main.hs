import Data.List (sortBy)
import Data.Ord (comparing)
import System.IO (readFile)

type Candidate = String

type Vote = Int

type CandidateVote = (Candidate, Vote)

type Representative = Int

type State = (Representative, [CandidateVote])

parseCandidates :: String -> [Candidate]
parseCandidates = lines

parseStates :: String -> [Candidate] -> [State]
parseStates input candidates = votes
  where
    states = (map words . lines) input
    votes = [(read (head x), votes) | x <- states, let votes = zip candidates ((map read . tail) x)]

diff :: (Num a) => a -> a -> a
a `diff` b = abs $ a - b

addVote :: Candidate -> [CandidateVote] -> Vote -> [CandidateVote]
addVote _ [] _ = []
addVote name ((candidate, votes) : candidates) addedVotes
  | name == candidate = (candidate, votes + addedVotes) : addVote name candidates addedVotes
  | otherwise = (candidate, votes) : addVote name candidates addedVotes

-- addVotes :: State -> [CandidateVote]
addVotes (representatives, votes) = candidateVotes
  where
    -- candidateVotes = map (\(name, v) -> (name, floor ((fromIntegral v / fromIntegral totalVotes) * fromIntegral representatives))) votes
    totalVotes = sum $ map snd votes

    candidateVotes = map addVote votes

    missingRepresentatives = representatives - sum (map (\(_, v, _) -> v) candidateVotes)

    rests = sortBy (comparing (\(_, _, x) -> x)) candidateVotes

    addVote :: CandidateVote -> (Candidate, Vote, Double)
    addVote (name, v) = (name, representatives', rest)
      where
        votes = (fromIntegral v / fromIntegral totalVotes) * fromIntegral representatives
        representatives' = floor votes
        rest = votes `diff` fromIntegral representatives'

main :: IO ()
main = do
  kandidater <- readFile "kandidater'.txt"
  stater <- readFile "stater'.txt"
  let candidates = parseCandidates kandidater
      votes = parseStates stater candidates
  print candidates
  print $ head votes
  print $ addVotes (head votes)
