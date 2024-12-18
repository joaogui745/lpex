import Data.List (nub, sortBy)
-- Último elemento da lista
ultimo :: [a] -> a
ultimo = last

-- Função index
indice :: [a] -> Int -> a
indice (x:xs) 0 = x
indice (x:xs) n = indice xs (n-1)

-- indice xs n = xs !! n

--  Inversão de lista
inverter :: [a] -> [a]
inverter xs = go xs [] where
    go [] acc = acc
    go (x:xs) acc = go xs (x : acc)

-- Renomeando Função
--inverter = reverse

{-  Usando List Comprehension
inverter xs = [xs !! i | i <- [xslen - 1, xslen - 2 .. 0]] where
    xslen = length xs
-}
{-  Usando Concatenação
inverter [] = []
inverter (x:xs) = inverter xs ++ [x] 
-}

-- Sort Decrescente sem Repetições
ordenaDec :: [Int] -> [Int] -- BubleSort Decrescente sem repetições
ordenaDec xs = aux (buble xs) [] where -- Função aux possibilita a aplicação inicial
    aux :: [Int] -> [Int] -> [Int]
    aux [] acc = acc
    aux xs acc = aux (buble (init xs)) (last xs : acc)

buble :: [Int] -> [Int]
buble [] = []
buble [e] = [e]
buble (x:y:xs)
    | x < y = y : buble (x:xs)
    | x > y = x : buble (y:xs)
    | otherwise = buble (y:xs)

{-  Usando função Sort
ordenaDec xs = sortBy (flip compare) (nub xs)
-}

-- Indica se a lista está decrescente
isDec :: [Int] -> Bool
{-
isDec [] = True
isDec [x] = True
isDec (x:y:xs) = x >= y && isDec(y:xs)
-}

{-  Usando Sort
isDec xs = compara xs == xs where
    compara :: [Int] -> [Int]
    compara = sortBy (flip compare)
-}

-- Usando Fold, Map e Zip
--isDec xs = foldl (&&) True (map (\(x, y) -> x > y) (zip xs (tail xs))) -- Zip interessante em operações intra-elementares

isDec xs = and (zipWith (>) xs (tail xs))


-- Verificar Ascendência usando Fold
isSorted (x:xs) = fst $ foldl step (True, x) xs
  where step (b, x) y = (b && (x <= y), y)

quickSort [] = []
quickSort (pivo:ls) = quickSort menores ++ [pivo] ++ quickSort maiores where
    menores = [men| men <- ls, men < pivo]
    maiores = [mai | mai <- ls, mai > pivo]


mergeSort [] = []
mergeSort [x] = [x]
mergeSort ls = merge (mergeSort esquerda) (mergeSort direita) where
    (esquerda, direita) = splitAt (div (length ls) 2) ls
    merge (x:xs) (y:ys)
        | x <= y = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys
    merge [x] ys = ys
    merge xs [] = xs