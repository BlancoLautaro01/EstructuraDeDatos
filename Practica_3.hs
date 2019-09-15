import Practica_1 
-- PRACTICA 3 --
----------------
-- ARBOLES BINARIOS --
----------------------

-- 1 --
-------
-- Funciones Concretas --
-------------------------
-- Definicion de Tipos --
-------------------------
data Tree a = EmptyT
			| Node a (Tree a) (Tree a)
			deriving (Show, Eq)

-- Instancias --
----------------
tree0 :: Tree Int
tree0 = Node 4 EmptyT EmptyT

tree1 :: Tree Int
tree1 = Node 1 EmptyT EmptyT

tree2 :: Tree Int
tree2 = Node 2 (tree1) (tree0)

tree3 :: Tree Int
tree3 = Node 4 (tree1) (tree2)

tree4 :: Tree Int
tree4 = Node 8 (tree3) (tree3)

treeS :: Tree String
treeS = Node "Lautaro" 
			(Node "Carlos" EmptyT EmptyT)
			(Node "Agustin" EmptyT EmptyT)

-- Funciones --
---------------
-- 1 --
-------
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (Node x t1 t2) = x + (sumarT t1) + (sumarT t2)

-- 2 --
-------
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (Node x t1 t2) = 1 + (sizeT t1) + (sizeT t2)

-- 3 --
-------
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (Node x t1 t2) = (Node (x*2) (mapDobleT t1) (mapDobleT t2))

-- 4 --
-------
mapLongitudT :: Tree String -> Tree Int
mapLongitudT EmptyT = EmptyT
mapLongitudT (Node x t1 t2) = Node (longitud x) (mapLongitudT t1) (mapLongitudT t2)

-- 5 --
-------
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (Node y t1 t2) = (x == y) || (perteneceT x t1) || (perteneceT x t2)

-- 6 --
-------
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x EmptyT = 0
aparicionesT x (Node y t1 t2) = 
	if (x == y)
		then 1 + (aparicionesT x t1) + (aparicionesT x t2)
		else (aparicionesT x t1) + (aparicionesT x t2)

-- 7 --
-------
countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (Node x EmptyT EmptyT) = 1
countLeaves (Node x t1 EmptyT) = countLeaves t1
countLeaves (Node x EmptyT t2) = countLeaves t2
countLeaves (Node x t1 t2) = (countLeaves t1) + (countLeaves t2)
		
-- 8 --
-------
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (Node x EmptyT EmptyT) = [x]
leaves (Node x t1 t2) = (leaves t1) ++ (leaves t2)

-- 9 --
-------
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (Node x t1 t2) = 
	1 + max (heightT t1) (heightT t2)

-- 10 --
--------
countNotLeaves :: Tree a -> Int
-- Con recursion
countNotLeaves EmptyT = 0
countNotLeaves (Node x EmptyT EmptyT) = 0
countNotLeaves (Node x t1 t2) = 1 + (countNotLeaves t1) + (countNotLeaves t2)

countNotLeaves' :: Tree a -> Int
-- Sin recursion
countNotLeaves' x = ((sizeT x) - (countLeaves x))

-- 11 --
--------
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (Node x t1 t2) = (Node x (mirrorT t2) (mirrorT t1))

-- 12 --
--------
listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (Node x t1 t2) = (listInOrder t1) ++ (x: listInOrder t2)

-- 13 --
--------
listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (Node x t1 t2) = (x: listPreOrder t1) ++ (listPreOrder t2)

-- 14 --
--------
listPostOrder :: Tree a -> [a]
listPostOrder EmptyT = []
listPostOrder (Node x t1 t2) = (listPostOrder t1) ++ (listPostOrder t2) ++ [x]

-- 15 --
--------
concatenarListasT :: Tree [a] -> [a]
concatenarListasT EmptyT = []
concatenarListasT (Node xs t1 t2) = (concatenarListasT t1) ++ xs ++ concatenarListasT t2

-- 16 --
--------
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (Node x t1 t2) = [x]
levelN n (Node x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)

-- 17 --
--------
-- listPerLevel :: Tree a -> [[a]]
-- listPerLevel EmptyT = []
-- listPerLevel (Node x t1 t2) =

-- 18 --
ramaMasLarga :: Tree a -> [a]
-- De tener mas de una rama maxima, indica la de mas a la izquierda.
ramaMasLarga EmptyT = []
ramaMasLarga (Node x t1 t2) = 
	if (heightT t1 >= heightT t2)
		then (x: ramaMasLarga t1)
		else (x: ramaMasLarga t2)

-- 19 --
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (Node x t1 t2) = x (todosLosCaminos t1) (todosLosCaminos t2)