import Queue

-- Funciones --
buildQueue :: [a] -> Queue a
buildQueue [] = emptyQ
buildQueue (x:xs) = queue x (buildQueue xs)

-- 1 --
lengthQ :: Queue a -> Int
lengthQ queue = 
	if isEmptyQ queue
		then 0
		else 1 + (lengthQ (deQueue queue))

-- 2 --
queueToList :: Queue a -> [a]
queueToList queue = 
	if isEmptyQ queue
		then []
		else ((firstQ queue) : (queueToList (deQueue queue)))

-- 3 --
unionQ :: Queue a -> Queue a -> Queue a
unionQ queue1 queue2 = buildQueue ((queueToList queue2) ++ (queueToList queue1))