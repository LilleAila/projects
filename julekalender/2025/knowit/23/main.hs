{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Square = Start | Open | Wall | End deriving (Show, Eq)

type Grid = [[Square]]

data Coord = Coord Int Int deriving (Show, Eq, Ord)

parseSquare :: Char -> Square
parseSquare 'A' = Start
parseSquare 'o' = Open
parseSquare 'x' = Wall
parseSquare 'B' = End
parseSquare _ = Open

h :: Grid -> Int
h = length

w :: Grid -> Int
w = length . head

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
    dcs = [Coord 1 0, Coord 0 1, Coord 1 1]

findCoord :: Grid -> Square -> Maybe Coord
findCoord g t =
  listToMaybe
    [ Coord y x
      | y <- [0 .. h g - 1],
        x <- [0 .. w g - 1],
        g !! y !! x == t
    ]

solve :: Grid -> Maybe Integer
solve g = do
  Coord sy sx <- findCoord g Start
  Coord ey ex <- findCoord g End
  pure (grid !! sy !! sx)
  where
    height = h g
    width = w g

    grid :: [[Integer]]
    grid =
      [ [paths (Coord y x) | x <- [0 .. width - 1]]
        | y <- [0 .. height - 1]
      ]

    paths :: Coord -> Integer
    paths c@(Coord y x)
      | not (valid g c) = 0
      | g !! y !! x == Wall = 0
      | g !! y !! x == End = 1
      | otherwise = sum [grid !! y' !! x' | Coord y' x' <- neighbors g c]

main :: IO ()
main = do
  grid <- map (map parseSquare) . lines <$> readFile "input.txt"
  print $ solve grid
