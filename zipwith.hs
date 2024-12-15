import Prelude hiding (zipWith)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith func (x:xs) (y:ys) = func x y : zipWith func xs ys
zipWith func _ _ = []