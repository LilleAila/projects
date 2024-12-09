import Data.List (isPrefixOf)
import System.IO (readFile)

numbers :: [(String, Int)]
-- numbers = [("null", 0), ("seks", 6), ("tolv", 12), ("atten", 18), ("tjuefire", 24), ("tretti", 30), ("trettiseks", 36), ("førtito", 42), ("førtiåtte", 48), ("femtifire", 54), ("seksti", 60), ("sekstiseks", 66), ("syttito", 72), ("syttiåtte", 78), ("åttifire", 84), ("nitti", 90), ("nittiseks", 96)]
numbers = [("seks", 6), ("tolv", 12), ("atten", 18), ("tjuefire", 24), ("tretti", 30), ("førtito", 42), ("førtiåtte", 48), ("femtifire", 54), ("seksti", 60), ("syttito", 72), ("syttiåtte", 78), ("åttifire", 84), ("nitti", 90)]

decode :: String -> Maybe [Int]
decode [] = Just []
decode text = case matches of
  [] -> Nothing
  m -> tryMatches m
  where
    matches = [x | x@(n, _) <- numbers, n `isPrefixOf` text]

    tryMatches :: [(String, Int)] -> Maybe [Int]
    tryMatches [] = Nothing
    tryMatches ((name, value) : xs) = case decode (drop (length name) text) of
      Just result -> Just (value : result)
      Nothing -> tryMatches xs

main :: IO ()
main = do
  input <- readFile "tall.txt"
  print $ fmap ((`div` 6) . sum) (decode input)
