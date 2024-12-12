import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)

month :: Day -> Int
month date = month'
  where
    (_, month', _) = toGregorian date

day :: Day -> Int
day date = day'
  where
    (_, _, day') = toWeekDate date

advent :: Day -> Bool
advent date = date >= start && date <= end
  where
    (year, _, _) = toGregorian date
    start' = fromGregorian year 12 1
    start = addDays (-(fromIntegral (day start' - 1))) start'
    end = fromGregorian year 12 24

getBookmarks :: [Int] -> Day -> Int -> Int
getBookmarks [] date p = getBookmarks [1] (addDays 1 date) p
getBookmarks xs@[x1] date p = getBookmarks (x1 : xs) (addDays 1 date) p
getBookmarks xs@(x1 : x2 : _) date p
  | date > fromGregorian 2024 12 12 = sum xs
  | problemDay = getBookmarks (1 : 0 : xs) (addDays 2 date) 2
  | otherwise = getBookmarks ((x1 + x2) : xs) (addDays 1 date) (p - if update then 1 else 0)
  where
    update = day date == 7
    problemMonth = month date == 7 || advent date
    problemDay = if problemMonth then update else p <= 0 && update

main :: IO ()
main = do
  let start = fromGregorian 2020 4 1
  -- let end = fromGregorian 2024 12 12
  -- let days = [start .. end] -- Inclusive
  print $ getBookmarks [] start 0
