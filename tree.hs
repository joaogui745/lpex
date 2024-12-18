data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq, Ord, Show)

soma NilT = 0
soma (Node e arvoreEsq arvoreDir) = e + soma arvoreEsq + soma arvoreDir

foldTree :: (t -> t -> t) -> Tree t -> t -> t
foldTree _ NilT acc = acc
foldTree func (Node e arvEsq arvDir) acc = func (func (foldTree func arvEsq acc) e ) (foldTree func arvDir acc)  


arvore = Node [0] (Node [1] (Node [2] NilT NilT) (Node [3] NilT NilT)) (Node [4] (Node [5]NilT NilT) (Node [6] NilT NilT))
arvore2 = Node 0 (Node 1 (Node 2 NilT NilT) (Node 3 NilT NilT)) (Node 4 (Node 5 NilT NilT) (Node 6 NilT NilT))
result = foldTree (++) arvore []

arvore3 = Node 1 (Node 5 NilT NilT) (Node 5 NilT NilT)
somaTree NilT = 0
somaTree (Node valor nodeEsq nodeDir) = valor + somaTree nodeEsq + somaTree nodeDir
