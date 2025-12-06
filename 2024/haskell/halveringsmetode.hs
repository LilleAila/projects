-- Finner nullpunkt til en funksjon ved hjelp av halveringsmetoden
-- Dette virker kun hvis grafen krysser x-aksen, ikke for eksempel f(x) = x^2
-- Args: start, stop, funksjon, antall halvveringer
nullpunkt :: (Fractional a, Eq a, Ord a) => a -> a -> (a -> a) -> a -> a
nullpunkt a b f n
  | n == 0 = m
  | f a * f m < 0 = nullpunkt a m f $ n - 1
  | otherwise = nullpunkt m b f $ n - 1
  where
    m = (a + b) / 2

main :: IO ()
main = print $ nullpunkt 1 2 (\x -> 3 ** x - x - 3) 10

-- Notes for using ghci

-- $ ghci
-- ghci> :l halveringsmetode.hs
-- ghci> main
-- ghci> :q
