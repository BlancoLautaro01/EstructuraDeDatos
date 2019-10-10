import Set
import Tree



tree1 = Node (listToSet [1,4,2,3,4,6,6,3])
			(Node (listToSet [1,4,8,56,78,34]) EmptyT EmptyT)
			(Node (listToSet [23,23,45,53,2,42,13,132]) EmptyT EmptyT)

-- Funciones --
-- 1 --
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set1 = []
losQuePertenecen (x:xs) set1 = 
	if (belongS x set1)
		then (x: (losQuePertenecen xs set1))
		else (losQuePertenecen xs set1)

-- 2 --
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (listToSet xs)

listToSet :: Eq a => [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = (addS x (listToSet xs))

-- 3 --
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (Node set treeL treeR) = unionS set (unionS (unirTodos treeL) (unirTodos treeR))
