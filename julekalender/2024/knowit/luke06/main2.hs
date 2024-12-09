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
  | d1 == d2 =
      let display = displays !! d1
          display' = take s1 display ++ [display !! s2] ++ drop (s1 + 1) (take s2 display ++ [display !! s1] ++ drop (s2 + 1) display)
       in take d1 displays ++ [display'] ++ drop (d1 + 1) displays
  | otherwise =
      let -- Assuming 0-indexing from the left. This contradicts the task, which asks for digits at the right.
          display1 = displays !! d1
          display2 = displays !! d2

          display1' = take s1 display1 ++ [display2 !! s2] ++ drop (s1 + 1) display1
          display2' = take s2 display2 ++ [display1 !! s1] ++ drop (s2 + 1) display2

          withd1 = take d1 displays ++ [display1'] ++ drop (d1 + 1) displays
          withd2 = take d2 withd1 ++ [display2'] ++ drop (d2 + 1) withd1
       in withd2

valid :: Int -> Bool
valid n = ac_ib && bd_eg && gf_de && ba_bb
  where
    displays = numberToDisplays n
    swap = swapSegments displays
    -- Manually translated to be from left and use 0-indexing
    ac_ib = displays == swap 8 2 0 1
    bd_eg = displays == swap 7 3 4 7
    gf_de = displays == swap 2 5 5 4
    ba_bb = displays == swap 7 0 7 1

main :: IO ()
main = do
  -- let numbers = [x | x <- [0 .. 999999999], valid x]
  let numbers = filter valid [0 .. 999999999]
  print $ length numbers
