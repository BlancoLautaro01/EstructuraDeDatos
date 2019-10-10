module Stack (
	Stack,
	emptyS,
	isEmptyS,
	push,
	top,
	pop,
	maxS
	) where

-- Implementacion --
-- Invariante de Representacion: El n elemento de la lista2 es el maximo cuando la lista1 tiene n elementos.
data Stack a = S [a] [a]
	deriving(Show)

-- Funciones --
-- 1 --
emptyS :: Stack a
emptyS = (S [] [])

-- 2 --
isEmptyS :: Stack a -> Bool
isEmptyS (S xs ys) = null xs

-- 3 --
{- Push para version sin maximo.
push :: a -> Stack a -> Stack a
push e (S xs) = (S (e:xs))
-}

-- 4 --
top :: Stack a -> a
top (S xs ys) = head xs

-- 5 --
{- Pop para version sin maximo.
pop :: Stack a -> Stack a
pop (S xs) = (S (tail xs))
-}

-- Pop para satifacer ejercicio 6.
pop :: Stack a -> Stack a
pop (S xs ys) = (S (tail xs) (tail ys))

-- Funciones ejercicio 6 --
-- 6 --
maxS :: Ord a => Stack a -> a
maxS (S xs ys) = head ys

-- 7 --
-- Push para satifacer ejercicio 6.
push :: Ord a => a -> Stack a -> Stack a
push e (S xs ys) = 
	if (null ys)
		then (S [e] [e])
		else (S (e:xs) (max e (head ys):ys))