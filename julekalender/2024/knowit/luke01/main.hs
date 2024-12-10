import Data.Char (digitToInt)
import Data.List (transpose)
import System.IO (readFile)

type Teppe = [[Bool]]

type Person = [[Int]]

rotate :: (Ord a) => [[a]] -> [[a]]
rotate = map reverse . transpose

parseTeppe :: String -> Teppe
parseTeppe = map parseLine . lines
  where
    parseLine l = take 5 $ map parseChar l
    parseChar ' ' = False
    parseChar 'x' = True

parsePerson :: String -> Person
parsePerson p = (map parseLine . lines) p ++ repeat (repeat 0)
  where
    parseLine l = map parseChar l ++ repeat 0
    parseChar ' ' = 0
    parseChar c = digitToInt c

-- Sum of all numbers intersecting with teppe
getPoints :: Person -> Teppe -> Int
getPoints person teppe = sum $ map (\row -> sum $ map (\col -> if teppe !! row !! col then person !! row !! col else 0) [0 .. length (teppe !! row) - 1]) [0 .. length teppe - 1]

-- Prepend a column of False, effectively moving by 1
moveX :: Teppe -> Teppe
moveX = map (False :)

-- Prepend a row of False, effectively moving by 1
moveY :: Teppe -> Teppe
moveY teppe = replicate (length (head teppe)) False : teppe

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

solve :: Person -> Teppe -> Int
solve person teppe = maximum [solve' teppe, solve' (rotate teppe), solve' (rotate (rotate teppe)), solve' (rotate (rotate (rotate teppe)))]
  where
    solve' teppe = tryY person teppe 0

main :: IO ()
main = do
  person <- readFile "joe.txt"
  teppe <- readFile "teppe.txt"
  let person' = parsePerson person
  let teppe' = parseTeppe teppe
  print $ solve person' teppe'
