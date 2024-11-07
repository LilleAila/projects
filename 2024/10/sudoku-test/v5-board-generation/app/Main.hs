-- Management of "random" numbers:
-- Parent randomGen with seed 1
-- For each board, use the head of parent randoms as the seed for a child random
-- Repeat for the next board, using the tail of the parent randomGen.
-- This ensures reproducability while using random numbers.

module Main (main) where

-- import Lib

import System.Random (mkStdGen, randomRs)

getRandoms :: Int -> (Int, Int) -> [Int]
getRandoms seed range = randomRs range (mkStdGen seed)

main :: IO ()
main = do
  print $ take 10 $ getRandoms 1 (1, 100)
