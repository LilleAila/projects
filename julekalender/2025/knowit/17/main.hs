import Data.List (sort)

newtype Snowball = Snowball Int deriving (Show, Eq, Ord)

data Snowman = Snowman
  { size :: Int,
    hat :: Bool,
    carrot :: Bool
  }
  deriving (Show, Eq)

data Command = Roll Int | Stack | Hat | Carrot deriving (Show, Eq)

parseCommand :: String -> Command
parseCommand "STACK" = Stack
parseCommand "HAT" = Hat
parseCommand "CARROT" = Carrot
parseCommand x = Roll (length (words x))

addSnowball :: [Snowball] -> [Snowman] -> ([Snowball], [Snowman])
addSnowball snowballs snowmen = tryStack . sort $ snowballs
  where
    tryStack [] = ([], snowmen)
    tryStack (b : bs) =
      case b of
        Snowball 1 ->
          case break ((== 2) . size) snowmen of
            (before, s : after) -> (bs, before ++ updateSnowman s : after)
            (_, []) -> keepBall (tryStack bs)
        Snowball 2 ->
          case break ((== 1) . size) snowmen of
            (before, s : after) -> (bs, before ++ updateSnowman s : after)
            (_, []) -> keepBall (tryStack bs)
        Snowball 3 ->
          (bs, Snowman 1 False False : snowmen)
        _ -> keepBall (tryStack bs)
      where
        keepBall (remainingBalls, finalSnowmen) = (b : remainingBalls, finalSnowmen)
        updateSnowman (Snowman s h c) = Snowman (s + 1) h c

addHat :: [Snowman] -> [Snowman]
addHat snowmen = case break carrotNeedsHat snowmen of
  (before, Snowman s _ c : after) -> before ++ Snowman s True c : after
  _ -> case break needsHat snowmen of
    (_, []) -> snowmen
    (before, Snowman s _ c : after) -> before ++ Snowman s True c : after
  where
    needsHat x = size x == 3 && not (hat x)
    carrotNeedsHat x = needsHat x && carrot x

addCarrot :: [Snowman] -> [Snowman]
addCarrot snowmen = case break hatNeedsCarrot snowmen of
  (before, Snowman s h _ : after) -> before ++ Snowman s h True : after
  _ -> case break needsCarrot snowmen of
    (_, []) -> snowmen
    (before, Snowman s h _ : after) -> before ++ Snowman s h True : after
  where
    needsCarrot x = size x == 3 && not (carrot x)
    hatNeedsCarrot x = needsCarrot x && hat x

rollSnowball :: Int -> [Snowball] -> [Snowman] -> ([Snowball], [Snowman])
rollSnowball x snowballs snowmen
  | x == 3 = (snowballs, Snowman 1 False False : snowmen)
  | otherwise = (Snowball x : snowballs, snowmen)

go :: [Command] -> [Snowball] -> [Snowman] -> ([Snowball], [Snowman])
go [] snowballs snowmen = (snowballs, snowmen)
go (x : xs) snowballs snowmen = case x of
  Roll x -> go xs snowballs' snowmen'
    where
      (snowballs', snowmen') = rollSnowball x snowballs snowmen
  Stack -> go xs snowballs' snowmen'
    where
      (snowballs', snowmen') = addSnowball snowballs snowmen
  Hat -> go xs snowballs snowmen'
    where
      snowmen' = addHat snowmen
  Carrot -> go xs snowballs snowmen'
    where
      snowmen' = addCarrot snowmen

finished :: Snowman -> Bool
finished s = size s == 3 && hat s && carrot s

main :: IO ()
main = do
  commands <- map parseCommand . lines <$> readFile "input.txt"
  -- let commands = [Roll 3, Roll 2, Stack, Roll 1, Stack, Carrot, Roll 3, Roll 2, Stack, Roll 1, Stack, Hat]
  let (snowballs, snowmen) = go commands [] []
  -- print snowballs
  -- print snowmen
  print $ length . filter finished $ snowmen
