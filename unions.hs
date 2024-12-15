unionRep :: [a] -> [a] -> [a]
unionRep a b = a ++ b

unionSet :: Eq a => [a] -> [a] -> [a]
unionSet xs ys = [x | x <- xs, x `notElem` ys] ++ ys

-- Interessante: implementação recursiva usando tail recursion

