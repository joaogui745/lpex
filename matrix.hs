somaMatricial :: [[Int]] -> [[Int]] -> [[Int]]
--somaMatricial = zipWith (zipWith (+))

{-
somaMatricial a b = [ [ ((a !! i) !! j) + ((b !! i) !! j) | j <- [0 .. tmn_col]] | i <- [0 .. tmn_linha]] where
    tmn_linha = length a - 1
    tmn_col = length (head a) - 1
-}

somaMatricial ax bx = [ [a + b | (a, b) <- zip as bs]| (as, bs) <- zip ax bx]


--somaMatricial = (zipWith . zipWith) (+)


matrizTransposta :: [[Int]] -> [[Int]]
matrizTransposta ax = [[as !! i|as <- ax] | i <- [0 .. id_col]] where
    id_col = length (head ax) - 1

{-
matrizTransposta :: [[a]] -> [[a]]
matrizTransposta ([]:_) = []
matrizTransposta x = map head x : matrizTransposta (map tail x)
-}


multiplicacaoMatricial :: [[Int]] -> [[Int]] -> [[Int]]

{-
multiplicacaoMatricial ax bx = [[ sum (zipWith (*) as bs) | bs <- tbx] | as <- ax] where
    tbx = matrizTransposta bx
-}


multiplicacaoMatricial [] bx = []
multiplicacaoMatricial ax bx = go (head ax) tbx : multiplicacaoMatricial (tail ax) bx where
    go ax [] = []
    go ax bx = sum (zipWith (*) ax (head bx)) : go ax (tail bx)
    tbx = matrizTransposta bx


matrix :: [[Int]]
matrix = [[1,2,3],[4,5,6],[7,8,9]]
matrix2 :: [[Int]]
matrix2 = [[9,8,7], [6,5,4], [3,2,1]]
