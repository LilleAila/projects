{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (intercalate, minimumBy, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set

type Node = String

type Weight = Int

parseEdge :: [String] -> (Node, (Node, Weight))
parseEdge [a, b, w] = (a, (b, read w))

loop :: Map.Map Node (Node, Weight) -> Node -> Maybe [(Node, Weight)]
loop ns a' = loop' Set.empty a' (ns Map.! a') ns
  where
    loop' :: Set.Set Node -> Node -> (Node, Weight) -> Map.Map Node (Node, Weight) -> Maybe [(Node, Weight)]
    loop' visited a (b, w) ns
      | b == a' = Just [(b, w)]
      | b `Set.member` visited = Nothing
      | otherwise = ((b, w) :) <$> loop' (Set.insert b visited) b (ns Map.! b) ns

nodeToInt :: String -> Int
nodeToInt (_ : n) = read n

rotateMin ns = take n . drop i . cycle $ ns
  where
    n = length ns
    i = snd $ minimumBy (comparing (nodeToInt . fst . fst)) $ zip ns [0 ..]

time = sum . map snd

format :: [(Node, Weight)] -> String
format ns = nodes ++ " (" ++ show (time ns) ++ ")"
  where
    nodes = intercalate " -> " . map fst $ ns ++ take 1 ns

main :: IO ()
main = do
  nodes <- Map.fromList . map (parseEdge . words) . lines <$> readFile "input.txt"
  let loops = sortOn time . nub . mapMaybe (fmap rotateMin . loop nodes) . Map.keys $ nodes
  putStrLn $ format (head loops)
