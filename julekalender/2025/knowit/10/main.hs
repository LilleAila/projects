import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

type Machine = (String, Float, Float, Float)

type MachineMap = Map.Map String Int

split :: String -> [String]
split [] = [""]
split (',' : ' ' : xs) = "" : split xs
split (x : xs) =
  let (h : ts) = split xs
   in (x : h) : ts

parse :: [String] -> Machine
parse [name, temp, water, kullsyre] = (name, read temp, read water, read kullsyre)

run :: Machine -> Maybe Int
run (name, temp, water, kullsyre)
  | temp < 95 || temp > 105 = Nothing
  | water < 400 || water > 1500 = Nothing
  | kullsyre < 300 || kullsyre > 500 = Nothing
  | otherwise = Just $ floor (result - loss)
  where
    water' = water - 100
    result = water' + (kullsyre / 10)
    loss = fromIntegral . floor $ if temp >= 100 then result / 40 else 0

run' :: MachineMap -> Machine -> MachineMap
run' ms m@(name, _, _, _) = Map.insertWith (+) name value ms
  where
    value = fromMaybe 0 (run m)

main :: IO ()
main = do
  machines <- map (parse . split) . lines <$> readFile "input.txt"
  let result = Map.toList $ foldl run' Map.empty machines
  let maxMachine = maximumBy (comparing snd) result
  let total = sum . map snd $ result
  putStrLn $ show total ++ " " ++ fst maxMachine
