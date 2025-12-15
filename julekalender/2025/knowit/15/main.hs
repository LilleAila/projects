import Data.Bits (testBit)
import Data.List (intercalate)

main :: IO ()
main = do
  let totalChocolate = (2 ^ 24) - 1 -- Sum of geometric series
  let calendar1 = 167772150
  let calendar2 = 149848566
  let chocolateWeight = calendar1 `div` totalChocolate
  let mockolateWeight = chocolateWeight * 7 `div` 10
  let n :: Integer = (10 * totalChocolate - calendar2) `div` 3 -- Solved 7n + (a-n) * 10 = b for n
  let days = [i + 1 | i <- [0 .. 23], testBit n i]
  putStrLn $ "SjokoladeMg:" ++ show chocolateWeight ++ ",MockuladeMg:" ++ show mockolateWeight ++ ",AntallMockulader:" ++ show n ++ ",Luker:" ++ intercalate "," (map show days)
