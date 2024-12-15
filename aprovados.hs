import Data.List (sortBy)
aprovadosOrdemDeMedia :: [(String,Float,Float)] -> [(String,Float)]
aprovadosOrdemDeMedia ls = sortBy compara (calculaNota ls) where
    compara (a, b) (c, d)
        | b > d = LT
        | otherwise = GT 

calculaNota :: [(String, Float, Float)] -> [(String, Float)]
calculaNota ((s, n1, n2):xs)
    | media >= 5.0 = (s, media) : calculaNota xs
    | otherwise = calculaNota xs
    where media = (n1 + n2) / 2
calculaNota [] = []


{- Uso de Map, Filter e Sort
aprovadosOrdemDeMedia ls = sortBy compara (filter condicao (map calculaMedia ls)) where
    condicao = (>5.0) . snd
    compara (a, b) (c, d)
        | b > d = LT
        | otherwise = GT 
-}
calculaMedia :: (String,Float,Float) -> (String,Float)
calculaMedia (nome, n1, n2) = (nome, (n1 + n2) / 2)

-- [("João", 2.0, 7.0), ("Tiago", 9.4, 8.7), ("Julia", 5.4, 8.3), ("Rafa", 1.9, 6.9)]