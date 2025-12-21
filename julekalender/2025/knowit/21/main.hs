import Data.Char (toLower)
import Data.List (elemIndex)

alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZÆØÅ"

alphabetIndex x = case elemIndex x alphabet of
  Just x -> x
  Nothing -> error "wtf"

shift :: Char -> Char -> Char
shift p c = alphabet !! ((alphabetIndex c - alphabetIndex p) `mod` length alphabet)

getKey :: String -> String -> String
getKey = zipWith shift

decode :: String -> String -> String
decode key = zipWith shift (cycle key)

main :: IO ()
main = do
  putStrLn $ getKey "RUDOLF" "VÆFUPO"
  putStrLn $ getKey "RØDTOGGRØNT" "VEFZSPIUHRZ"
  let key = "EGCGEJCDJ"
  putStrLn $ decode key "JUTGRPCUJWPGTXRNQRWYGT"
