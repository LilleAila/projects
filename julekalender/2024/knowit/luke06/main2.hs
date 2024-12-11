-- > time ./main2
-- 134976000
-- ./main2  1800,91s user 11,46s system 100% cpu 30:12,01 total
-- (30 minutes)

import Data.Char (digitToInt)

type Segment = Bool

type Number = [Segment]

type Display = Number

numbers :: [Number]
numbers = map (map (== '.')) ["......-", "-..----", "..-..-.", "....--.", "-..--..", ".-..-..", ".-.....", "...----", ".......", "....-.."]

numberToDisplays :: Int -> [Display]
numberToDisplays n = zeros ++ number
  where
    zeros = replicate (9 - length (show n)) (head numbers)
    number = (map (\n -> numbers !! digitToInt n) . show) n

swapSegments :: [Display] -> Int -> Int -> Int -> Int -> [Display]
swapSegments displays d1 s1 d2 s2
  | d1 == d2 -- Swap segments within the same display
    =
      let display = displays !! d1
          -- Swap the segments s1 and s2
          display' = take s1 display ++ [display !! s2] ++ drop (s1 + 1) (take s2 display ++ [display !! s1] ++ drop (s2 + 1) display)
       in take d1 displays ++ [display'] ++ drop (d1 + 1) displays
  | otherwise -- Swap segments between two displays
    =
      let display1 = displays !! d1
          display2 = displays !! d2
          -- Swap the segments s1 and s2 between the two displays
          display1' = take s1 display1 ++ [display2 !! s2] ++ drop (s1 + 1) display1
          display2' = take s2 display2 ++ [display1 !! s1] ++ drop (s2 + 1) display2
          updatedDisplays = take d1 displays ++ [display1'] ++ drop (d1 + 1) displays
       in take d2 updatedDisplays ++ [display2'] ++ drop (d2 + 1) updatedDisplays

valid :: Int -> Bool
valid n = displays == ba_bb
  where
    displays = numberToDisplays n
    -- swap = swapSegments displays
    -- Manually translated to be from left and use 0-indexing
    ac_ib = swapSegments displays 8 2 0 1
    bd_eg = swapSegments ac_ib 7 3 4 6
    gf_de = swapSegments bd_eg 2 5 5 4
    ba_bb = swapSegments gf_de 7 0 7 1

main :: IO ()
main = do
  -- let numbers = [x | x <- [0 .. 999999999], valid x]
  let numbers = filter valid [0 .. 999999999]
  print $ length numbers
