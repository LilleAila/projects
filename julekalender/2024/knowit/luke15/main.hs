import Data.Map qualified as Map
import Data.Maybe (fromJust)

numbers :: Map.Map Char Int
numbers = Map.fromList [ ('J', 10_000), ('U', 5_000), ('L', 1_000), ('E', 500), ('T', 100), ('R', 50), ('3', 10), ('V', 5), ('I', 1) ]

validate :: [Int] -> Maybe Int -> Maybe Int
validate [] _ = Just 0
validate (x: xs) prev
  | isDescending = fmap (x +) (validate xs (Just x))
  | otherwise = Nothing
  where
    isDescending = case prev of
      Nothing -> True
      Just p -> x <= p

-- parseNumber :: String -> Maybe Int
parseNumber n = validate digits Nothing
  where
    digits = fromJust (traverse (`Map.lookup` numbers) n)

main :: IO ()
main = do
  print $ parseNumber "R33VIE"
  print $ parseNumber "TUR33VIE"
  print $ parseNumber "3R"
  print $ parseNumber "JULEL"
