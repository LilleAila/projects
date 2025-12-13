import Data.List (intercalate, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Recipe = Recipe String Int [String] deriving (Show, Eq)

type StepsMap = Map.Map String Int

type StepsSet = Set.Set String

parseRecipe :: [String] -> Recipe
parseRecipe (name : joy : steps) = Recipe name (read joy) steps

parseStep :: [String] -> (String, Int)
parseStep [name, time] = (name, read time)

getTime :: StepsSet -> StepsMap -> Int
getTime steps stepTimes = sum [stepTimes Map.! s | s <- Set.toList steps]

knapsack :: Int -> StepsMap -> StepsSet -> [Recipe] -> (Int, [Recipe])
knapsack _ _ _ [] = (0, [])
knapsack max stepTimes steps (r@(Recipe _ joy recipeSteps) : rs)
  | time > max = next
  | j2 > j1 = (j2, r : r2)
  | otherwise = next
  where
    steps' = Set.union steps (Set.fromList recipeSteps)
    time = getTime steps' stepTimes
    next@(j1, r1) = knapsack max stepTimes steps rs
    (j2', r2) = knapsack max stepTimes steps' rs
    j2 = j2' + joy

main :: IO ()
main = do
  let t = 45
  recipes <- map (parseRecipe . words) . lines <$> readFile "cookbook.txt"
  stepTimes <- Map.fromList . map (parseStep . words) . lines <$> readFile "steps.txt"
  let (joy, rs) = knapsack t stepTimes Set.empty recipes
  let rs' = sort $ map (\(Recipe name _ _) -> name) rs
  putStrLn $ intercalate "," (show joy : rs')
