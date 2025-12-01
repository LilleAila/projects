import Data.List (uncons)

splitCommand :: String -> (String, Maybe String)
splitCommand x =
  case break (== ' ') x of
    (a, "") -> (a, Nothing)
    (a, _ : bs) -> (a, Just bs)

head' xs = case uncons xs of
  Just (x, _) -> x
  Nothing -> error "wtf"

handleCommand :: ([String], String) -> (String, Maybe String) -> ([String], String)
handleCommand (stack, key) command =
  -- NOTE: This is O(n) but could be done O(1) if using reversed lists and : instead of ++
  case command of
    ("ADD", Just x) -> (stack ++ [x], key)
    ("PROCESS", _) -> (drop 1 stack, key ++ [head' (head' stack)])
    ("COUNT", _) -> (stack, key ++ [(last . show . length) stack])

main :: IO ()
main = do
  content <- readFile "input.txt"
  let commands = map splitCommand $ lines content
  let result = foldl handleCommand ([], "") commands
  print $ snd result
