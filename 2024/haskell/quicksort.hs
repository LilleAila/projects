-- Recursive quicksort algorithm
-- Takes in a lise of orderable, then splits into lists of smaller than and bigger than x (the current element), using predicates so filter them, then run the parent functions on the two resulting lists
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
