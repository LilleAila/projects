import Data.Char (digitToInt)
import Data.List (transpose)
import System.IO (readFile)

type Teppe = [[Int]]

type Person = [[Int]]

rotate :: (Ord a) => [[a]] -> [[a]]
rotate = map reverse . transpose

parseTeppe :: String -> Teppe
parseTeppe = map parseLine . lines
  where
    parseLine l = take 5 $ map parseChar l
    parseChar :: Char -> Int
    parseChar ' ' = 0
    parseChar x = digitToInt x

parsePerson :: String -> Person
parsePerson p = (map parseLine . lines) p ++ repeat (repeat 0)
  where
    parseLine l = map parseChar l ++ repeat 0
    parseChar ' ' = 0
    parseChar 'x' = -2
    parseChar c = digitToInt c

getPoints :: Person -> Teppe -> Int
getPoints person teppe = sum $ map sum $ zipWith (zipWith (*)) teppe person

-- getPoints person teppe = sum t_points * sum p_points
--   where
--     points = concatMap (filter (\(t, p) -> t > 0)) (zipWith zip teppe person)
--     (t_points, p_points) = unzip points

moveX :: Teppe -> Teppe
moveX = map (0 :)

moveY :: Teppe -> Teppe
moveY teppe = replicate (length (head teppe)) 0 : teppe

tryX :: Person -> Teppe -> Int -> Int
tryX person teppe p = if points == 0 then 0 else max points $ tryX person movedTeppe movedPoints
  where
    points = getPoints person teppe
    movedTeppe = moveX teppe
    movedPoints = getPoints person movedTeppe

tryY :: Person -> Teppe -> Int -> Int
tryY person teppe p = if points == 0 then 0 else max points $ tryY person movedTeppe movedPoints
  where
    points = tryX person teppe 0
    movedTeppe = moveY teppe
    movedPoints = tryX person movedTeppe 0

permutations teppe = rotations teppe ++ rotations (map reverse teppe)
  where
    rotations :: Teppe -> [Teppe]
    rotations = take 4 . iterate rotate

solve :: Person -> Teppe -> Int
solve person = maximum . map solve' . permutations
  where
    solve' teppe = tryY person teppe 0

main :: IO ()
main = do
  person <- readFile "joe.txt"
  teppe <- readFile "teppe.txt"
  let person' = parsePerson person
  let teppe' = parseTeppe teppe
  print $ solve person' teppe'

-- print $ getPoints person' teppe'
