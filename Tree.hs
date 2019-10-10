module Tree (Tree) where

data Tree a =
	EmptyT
	| Node a (Tree a) (Tree a)
	deriving (Show, Eq)

isEmptyT :: Tree a -> Bool
isEmptyT EmptyT = True
isEmptyT (Node x t1 t2) = False