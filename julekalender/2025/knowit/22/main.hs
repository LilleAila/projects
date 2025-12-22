import Data.Char (ord)

lineScore xs i = magic - krampus
  where
    magic = i * (length . filter (`elem` "Md+")) xs
    krampus = sum . map ord . filter (`notElem` "Md+") $ xs

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ sum $ zipWith lineScore input [1 ..]
