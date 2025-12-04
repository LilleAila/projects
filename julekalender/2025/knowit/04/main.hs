data Tile = Snow | Bush | Deepsnow | Ice | Gingerbread deriving (Show, Eq)

parseContent = map parseContent' . init
  where
    parseContent' 'S' = Snow
    parseContent' 'B' = Bush
    parseContent' 'D' = Deepsnow
    parseContent' 'I' = Ice
    parseContent' 'P' = Gingerbread

solve e ys [] = 0
solve e ys (x : xs)
  | e' >= 0 = 10 + solve e' (de : ys) xs
  | otherwise = 0
  where
    e' = e + de
    de = case x of
      Snow -> -5
      Bush -> -10
      Deepsnow -> -15
      Ice -> 0
      Gingerbread -> if all (< 0) ys' then -(2 * sum ys') else 0
        where
          ys' = take 2 ys

main :: IO ()
main = do
  content <- readFile "track.txt"
  print $ solve 3000 [] $ parseContent content
