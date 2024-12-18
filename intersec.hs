intersec :: Eq a => [a] -> [a] -> [a]
-- List Comprehension
--intersec xs ys = [x | x <- xs, x `elem` ys]
-- intersec xs ys = [x | x <- xs, y <- ys, x == y]

-- Recursion
intersec [] ys = []
intersec (x:xs) ys 
    | x `elem` ys = x : intersec xs ys 
    | otherwise = intersec xs ys