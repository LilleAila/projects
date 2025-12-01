import Data.List (uncons)

data Command = Add String | Process | Count deriving (Show, Eq)

handleInput :: String -> Command
handleInput ('A' : 'D' : 'D' : ' ' : x) = Add x
handleInput ('P' : 'R' : 'O' : 'C' : 'E' : 'S' : 'S' : _) = Process
handleInput ('C' : 'O' : 'U' : 'N' : 'T' : _) = Count

head' xs = case uncons xs of
  Just (x, _) -> x
  Nothing -> error "wtf"

handleCommand :: ([String], String) -> Command -> ([String], String)
handleCommand (stack, key) command =
  case command of
    Add x -> (x : stack, key)
    Process -> (init stack, head' (last stack) : key)
    Count -> (stack, (last . show . length) stack : key)

main :: IO ()
main = do
  content <- readFile "input.txt"
  let commands = map handleInput $ lines content
  let result = foldl handleCommand ([], "") commands
  print $ reverse $ snd result
