main :: IO ()
main = do
  -- Manually squaring for efficiency
  print [a * b * c | a <- [1 .. 1000], b <- [a .. 1000], let c = 1000 - a - b, a * a + b * b == c * c]
