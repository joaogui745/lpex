data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Eq, Ord, Show)

soma NilT = 0
soma (Node e arvoreEsq arvoreDir) = e + soma arvoreEsq + soma arvoreDir

foldTree :: (t -> t -> t) -> Tree t -> t -> t
foldTree _ NilT acc = acc
foldTree func (Node e arvEsq arvDir) acc = (func e . func) (foldTree func arvEsq acc) (foldTree func arvDir acc)