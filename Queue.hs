module Queue (
	Queue,
	emptyQ,
	isEmptyQ,
	queue,
	firstQ,
	deQueue
	) where

-- Implementacion --
-- Invariante de Representacion: El Int representa la cantidad de elementos de [a].
data Queue a = Q [a] Int
			deriving (Show)

data QueueBis a = Qb [a]
			deriving (Show)

-- Funciones --
-- 1 --			
emptyQ :: Queue a
emptyQ = Q [] 0

emptyQ' :: QueueBis a
emptyQ' = Qb []

-- 2 --
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q xs n) = null xs

isEmptyQ' :: QueueBis a -> Bool
isEmptyQ' (Qb xs) = null xs

-- 3 --
queue :: a -> Queue a -> Queue a
queue x (Q xs n) = Q (xs ++ [x]) (n+1)

queue' :: a -> QueueBis a -> QueueBis a
queue' x (Qb xs) = Qb (x:xs)

-- 4 --
firstQ :: Queue a -> a
firstQ (Q xs n) = head xs

firstQ' :: QueueBis a -> a
firstQ' (Qb xs) = last xs

-- 5 --
deQueue :: Queue a -> Queue a
deQueue (Q xs n) = Q (tail xs) (n-1)

deQueue' :: QueueBis a -> QueueBis a
deQueue' (Qb xs) = Qb (init xs)

-- 6 --
lenQ :: Queue a -> Int
lenQ (Q xs n) = n