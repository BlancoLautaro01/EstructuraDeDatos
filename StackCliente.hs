import Stack
import Tree

-- Funciones --
-- 1 --
apilar :: Ord a => [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = (push x (apilar xs))

-- 2 --
desapilar :: Stack a -> [a]
desapilar stack = 
	if isEmptyS stack
		then []
		else ((top stack) : (desapilar (pop stack)))

-- 3 --
{-treeToStack :: Tree a -> Stack a
treeToStack tree = 
	if isEmptyT tree
		then emptyS
		else apilar (listTreeInOrder tree)-}

-- 4 --
balanceado :: String -> Bool
balanceado str = isEmptyS (chequearBalanceo str)

chequearBalanceo :: String -> Stack Char
chequearBalanceo [] = emptyS
chequearBalanceo (x:xs) = 
	if (x == ')')
		then push x (chequearBalanceo xs)
		else verSiPopear (x:xs)

verSiPopear :: String -> Stack Char
verSiPopear (x:xs)= 
	if ((x == '(') && (isEmptyS (chequearBalanceo xs)))
		then apilar ("False")
		else if (x == '(')
			then pop (chequearBalanceo xs)
			else chequearBalanceo xs