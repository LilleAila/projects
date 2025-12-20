data Box = Box Char Bool Int deriving (Show, Eq, Ord)

boxes =
  [ Box 'A' False 7,
    Box 'B' False 6,
    Box 'C' True 5,
    Box 'D' True 4,
    Box 'E' False 4,
    Box 'F' False 3,
    Box 'G' False 2
  ]

main :: IO ()
main = do
  putStrLn "idk"
