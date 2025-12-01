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
  case command of
    ("ADD", Just x) -> (x : stack, key)
    ("PROCESS", _) -> (init stack, head' (last stack) : key)
    ("COUNT", _) -> (stack, (last . show . length) stack : key)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let commands = map splitCommand $ lines content
  let result = foldl handleCommand ([], "") commands
  print $ reverse $ snd result
