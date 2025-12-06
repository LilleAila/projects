import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Square = Start | Open | Wall | End deriving (Show, Eq)

type Grid = [[Square]]

data Coord = Coord Int Int deriving (Show, Eq, Ord)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn s = foldr f [[]]
  where
    f x acc@(c : cs)
      | x == s = [] : acc
      | otherwise = (x : c) : cs

parseSquare :: Char -> Square
parseSquare 'S' = Start
parseSquare '.' = Open
parseSquare '#' = Wall
parseSquare '*' = End

head' xs = case listToMaybe xs of
  Just y -> y
  Nothing -> error "Nuh uh"

h :: Grid -> Int
h = length

w :: Grid -> Int
w = length . head'

valid :: Grid -> Coord -> Bool
valid g (Coord y x) = y >= 0 && y < h g && x >= 0 && x < w g

open :: Grid -> Coord -> Bool
open g (Coord y x) = case g !! y !! x of
  Wall -> False
  _ -> True

addCoord :: Coord -> Coord -> Coord
addCoord (Coord y x) (Coord y' x') = Coord (y + y') (x + x')

neighbors :: Grid -> Coord -> [Coord]
neighbors g c =
  [ c'
    | d <- dcs,
      let c' = c `addCoord` d,
      valid g c',
      open g c'
  ]
  where
    dcs = [Coord (-1) 0, Coord 1 0, Coord 0 (-1), Coord 0 1]

findCoord :: Grid -> Square -> Maybe Coord
findCoord g t =
  listToMaybe
    [ Coord y x
      | y <- [0 .. h g - 1],
        x <- [0 .. w g - 1],
        g !! y !! x == t
    ]

solve :: Grid -> Maybe Int
solve g = do
  s <- findCoord g Start
  e <- findCoord g End
  go g e Set.empty s
  where
    go :: Grid -> Coord -> Set Coord -> Coord -> Maybe Int
    go g goal visited c
      | c == goal = Just 0
      | null results = Nothing
      | otherwise = Just (1 + minimum results)
      where
        ns = filter (`Set.notMember` visited) $ neighbors g c
        results = catMaybes [go g goal (Set.insert c visited) n | n <- ns]

main :: IO ()
main = do
  zones <- map (map (map parseSquare)) . filter (/= []) . splitOn ";" . lines <$> readFile "input.txt"
  print $ (sum . mapMaybe solve) zones
