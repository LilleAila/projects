import Data.Char (digitToInt)
import Data.List (find)
import qualified Data.Set as Set
import Debug.Trace (trace)

sumSquares :: Int -> Int
sumSquares = sum . map ((^ 2) . digitToInt) . show

-- juletall :: Int -> Bool
-- juletall = isOne . iterate sumSquares
--   where
--     isOne (n : xs)
--       | n == 0 || n == 1 = True
--       | n `elem` take 100 xs = False
--       | otherwise = isOne xs

juletall :: Int -> Bool
juletall = checkJuletall Set.empty
  where
    checkJuletall seen n
      | n == 1 || n == 0 = True
      | n `Set.member` seen = False
      | otherwise = checkJuletall (n `Set.insert` seen) (sumSquares n)

get3s :: Int -> [Int]
get3s = map read . splitNumber 3 . show
  where
    splitNumber :: Int -> String -> [String]
    splitNumber size xs
      | length xs < size = []
      | otherwise = take size xs : splitNumber size (tail xs)

jule3tall :: Int -> Bool
jule3tall n = isJule3tall
  where
    numDigits = (length . show) n
    halfLength = numDigits `div` 2 -- + if odd numDigits then 1 else 0
    isJuletall = juletall n
    firstHalf = numDigits <= 1 || (juletall . read . take halfLength . show) n
    lastHalf = numDigits <= 1 || (juletall . read . reverse . take halfLength . reverse . show) n
    threes = (all juletall . get3s) n

    isJule3tall = isJuletall && firstHalf && lastHalf && threes

main :: IO ()
main = do
  -- print $ last $ filter jule3tall [0 .. 9999999]
  print $ find jule3tall [9999999, 9999998 .. 0]
