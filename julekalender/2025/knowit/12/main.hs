is :: String -> String -> Bool
is a b = a == b || a == reverse b

main :: IO ()
main = do
  items <- lines <$> readFile "input.txt"
  let bamse = "01001010"
  let togsett = "01000010"
  print $ length . filter (\(a, b) -> a `is` togsett && b `is` bamse) . zip items $ drop 1 items
  print $ length . filter (`is` bamse) $ items
