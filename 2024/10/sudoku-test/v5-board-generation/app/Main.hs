module Main (main) where

-- import Lib

import System.Random (mkStdGen, randomRs)

getRandoms :: Int -> (Int, Int) -> [Int]
getRandoms seed range = randomRs range (mkStdGen seed)

main :: IO ()
main = do
  print $ getRandoms 1 (1, 100)
