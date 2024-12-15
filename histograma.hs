-- Histograma da frequencia de letras
histograma :: [String] -> [(String,Int)]
histograma = foldr countstr [] where
    countstr :: String -> [(String,Int)] -> [(String,Int)]
    countstr s []
        | s == "" = []
        | otherwise = [(s, 1)]
    countstr s ((key, cout):xs)
        | s == key = (key, cout + 1) : countstr "" xs
        | otherwise = (key, cout) : countstr s xs


{-histograma :: [String] -> [(String,Int)]
histograma [] = []
histograma (x:xs) = countstr x (histograma xs)
-}