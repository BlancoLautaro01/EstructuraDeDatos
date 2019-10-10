module Set (
	Set,
	emptyS,
	addS,
	belongS,
	sizeS,
	removeS,
	unionS,
	intersectionS,
	setToList
) where

-- Implementacion --
{- Invariantes de Representacion para Set:
El entero tiene que representar la cantidad de elementos de la lista
La lista no debe tener repetidos
-}
data Set a = UnSet [a] Int
			deriving(Show)
			
-- Funciones --
-- 1 --
emptyS :: Set a
emptyS = UnSet [] 0

-- 2 --
addS :: Eq a => a -> Set a -> Set a
addS elem (UnSet xs n) = 
	if (pertenece elem xs) 
		then (UnSet xs n)
		else (UnSet (elem:xs) (n+1))

-- 3 --
belongS :: Eq a => a -> Set a -> Bool
belongS elem (UnSet xs n) = (pertenece elem xs)

-- 4 --
sizeS :: Eq a => Set a -> Int
sizeS (UnSet xs n) = n

-- 5 --
removeS :: Eq a => a -> Set a -> Set a
removeS elem (UnSet (x:xs) n) = 
	if elem == x
		then (UnSet xs (n-1))
		else addS x (removeS elem (UnSet xs (n-1)))

-- 6 --
unionS :: Eq a => Set a -> Set a -> Set a
unionS (UnSet xs n) (UnSet ys k) =
	(UnSet (unirListas xs ys) (length (unirListas xs ys)))

unirListas :: Eq a => [a] -> [a] -> [a]
unirListas [] [] = []
unirListas [] xs = xs
unirListas xs [] = xs
unirListas xs (y:ys) = 
	if (pertenece y xs)
		then unirListas xs ys
		else (y: (unirListas xs ys))

-- 7 --
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (UnSet xs n) (UnSet ys k) = 
	(UnSet (intersectarListas xs ys) (length (intersectarListas xs ys)))

intersectarListas :: Eq a => [a] -> [a] -> [a]
intersectarListas [] [] = []
intersectarListas [] xs = []
intersectarListas xs [] = []
intersectarListas xs (y:ys) = 
	if (pertenece y xs)
		then (y: (intersectarListas xs ys))
		else intersectarListas xs ys

-- 8 --
setToList :: Eq a => Set a -> [a]
setToList (UnSet xs n) = xs

-- Auxiliares
pertenece :: Eq a => a -> [a] -> Bool
pertenece elem [] = False
pertenece elem (x:xs) = (elem == x) || (pertenece elem xs)
