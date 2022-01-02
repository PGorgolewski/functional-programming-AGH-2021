import Distribution.Simple.PackageIndex (SearchResult(None))
data BinIntTree = EmptyIntBT | IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a)
sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | Add (Expr a) (Expr a) | Odd (Expr a) (Expr a) | Multiply (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Odd e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String 
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Odd e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Multiply e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt)  (depthOfBT rt)

flattenBTPreorder :: BinTree a -> [a]
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT n lt rt) = [n] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n lt rt) = flattenBTInorder lt ++ [n] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT n lt rt) = flattenBTPostorder lt ++ flattenBTPostorder rt ++ [n]

mapBT :: (a->b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt) = if a < n then NodeBT n (insert a lt) rt else NodeBT n lt (insert a rt)

