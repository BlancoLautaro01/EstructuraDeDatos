module Practica_1 (apariciones, 
					sumar,
					filtrarElemento,
					sumatoria,
					longitud,
					pertenece)
						where

-- PRACTICA 1 --

-----------------------
-- CONCEPTOS BASICOS --
-----------------------

-- 1.a --
sucesor :: Int -> Int
sucesor x = x + 1
-- 1.b --
sumar :: Int -> Int -> Int
sumar x y = x + y
-- 1.c --
maximo :: Int -> Int -> Int
maximo x y = 
	if x >= y
		then x
		else y

minimo :: Int -> Int -> Int
minimo x y = 
	if x <= y
		then x
		else y

-- 2.a --
negar :: Bool -> Bool
negar b = not b
-- 2.b --
andLogico :: Bool -> Bool -> Bool
andLogico b1 b2 = b1 && b2
-- 2.c --
orLogico :: Bool -> Bool -> Bool
orLogico b1 b2 = b1 || b2
-- 2.d --
primera :: (Int,Int) -> Int
primera (x,y) = x
-- 2.e --
segunda :: (Int,Int) -> Int
segunda (x,y) = y
-- 2.f --
sumaPar :: (Int,Int) -> Int
sumaPar (x,y) = sumar x y
-- 2.g --
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = maximo x y

-- 3.a --
loMismo :: a -> a
loMismo x = x
-- 3.b --
siempreSiete :: a -> Int
siempreSiete x = 7
-- 3.c --
duplicar :: a -> (a,a)
duplicar x = (x,x)
-- 3.d --
singleton :: a -> [a]
singleton x = [x]

-- 4.a --
isEmpty :: [a] -> Bool
isEmpty xs = null xs
-- 4.b --
head' :: [a] -> a
head' (x:xs) = x
-- 4.c --
tail' :: [a] -> [a]
tail' (x:xs) = xs

--------------------------
-- RECURSION CON LISTAS --
--------------------------

-- 1.1 --
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = sumar x (sumatoria xs)
-- 1.2 --
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = sumar 1 (longitud xs)
-- 1.3 --
mapSucesor :: [Int] -> [Int]
mapSucesor [] = []
mapSucesor (x:xs) = (sucesor x : mapSucesor xs)
-- 1.4 --
mapSumaPar :: [(Int, Int)] -> [Int]
mapSumaPar [] = []
mapSumaPar (x:xs) = (sumaPar x : mapSumaPar xs)
-- 1.5 --
mapMaxDelPar :: [(Int, Int)] -> [Int]
mapMaxDelPar [] = []
mapMaxDelPar (x:xs) = (maxDelPar x : mapMaxDelPar xs)
-- 1.6 --
-- ? ? --
todoVerdad :: [Bool] -> Bool
todoVerdad [] = True -- Porque es necesario esto.
todoVerdad [True] = True
todoVerdad [False] = False
todoVerdad (x1:x2:xs) = (andLogico x1 x2) && (todoVerdad xs)
-- 1.7 --
algunaVerdad :: [Bool] -> Bool
algunaVerdad [] = False
algunaVerdad (x:xs) = 
	if x then x else (algunaVerdad xs)
-- 1.8 --
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = 
	if (e == x)
		then True
		else (pertenece e xs)
-- 1.9 --
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = 
	if (e == x)
		then sumar 1 (apariciones e xs)
		else (apariciones e xs)
-- 1.10 --
filtrarMenoresA :: Int -> [Int] -> [Int]
filtrarMenoresA n [] = []
filtrarMenoresA n (x:xs) = 
	if n < x
		then (x : filtrarMenoresA n xs)
		else filtrarMenoresA n xs
-- 1.11 --
filtrarElemento :: Eq a => a -> [a] -> [a]
filtrarElemento e [] = []
filtrarElemento e (x:xs) = 
	if (e /= x)
		then (x : filtrarElemento e xs)
		else filtrarElemento e xs
-- 1.12 --
mapLongitudes :: [[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (x:xs) = (longitud x : mapLongitudes xs)
-- 1.13 --
longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA n [] = []
longitudMayorA n (x:xs) = 
	if (longitud x > n)
		then (x : longitudMayorA n xs)
		else longitudMayorA n xs
-- 1.14 --
intercalar :: a -> [a] -> [a]
intercalar e [] = []
intercalar e (x:xs) = 
	if longitud (x:xs) > 1
		then (x : e : intercalar e xs)
		else [x]
-- 1.15 --
snoc :: [a] -> a -> [a]
snoc [] e = [e]
snoc (x:xs) e = x:(snoc xs e) 
-- 1.16 --
append :: [a] -> [a] -> [a]
append [] [] = []
append xs [] = xs
append xs1 (x:xs2) = append (snoc xs1 x) xs2
-- 1.17 --
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = append x (aplanar xs)
-- 1.18 --
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = snoc (reversa xs) x
-- 1.19 --
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (x1:xs1) (x2:xs2) = (maximo x1 x2 : zipMaximos xs1 xs2)
-- 1.20 --
zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] [] = []
zipSort (x1:xs1) (x2:xs2) = (((minimo x1 x2),(maximo x1 x2)) : zipSort xs1 xs2)
-- 1.21 --
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = div (sumatoria xs) (longitud xs)
-- 1.22 --
minimum' :: Ord a => [a] -> a
minimum' [] = error "La lista no puede estar vacia"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

---------------------------
-- RECURSION CON NUMEROS --
---------------------------

-- 2.1 --
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)
-- 2.2 --
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 1 = [1]
cuentaRegresiva x = 
	if x < 1 
		then []
		else (x:cuentaRegresiva (x - 1))
-- 2.3 --
contarHasta :: Int -> [Int]
contarHasta 1 = [1]
contarHasta x = (x:contarHasta (x + 1))
-- 2.4 --
replicarN :: Int -> a -> [a]
replicarN 0 e = []
replicarN x e = (e:replicarN (x-1) e)
-- 2.5 --
desdeHasta :: Int -> Int -> [Int]
desdeHasta x y = 
	if (x == y)
		then [x]
		else (x:desdeHasta (x+1) (y))
-- 2.6 --
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n (x:xs) = 
	if n > (longitud (x:xs))
		then []
		else (x: takeN (n-1) xs)
-- 2.7 --
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n (x:xs) = 
	if n > (longitud (x:xs))
		then (x:xs)
		else dropN (n-1) xs
-- 2.8 --
-- No, no combiene usar recursion --
splitN :: Int -> [a] -> ([a], [a])
splitN n xs = (takeN n xs, dropN n xs)

----------------------------
-- EJERCICIOS ADICIONALES --
----------------------------

-- 1 --
maximum' :: Ord a => [a] -> a
maximum' [] = error "La lista no puede estar vacia"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
-- 2 --
splitMin :: Ord a => [a] -> (a, [a])
splitMin [e] = (e,[])
splitMin (x:xs) = joinSplitMin x (splitMin xs)

joinSplitMin :: Ord a => a -> (a, [a]) -> (a, [a])
joinSplitMin x (y,ys) = (min x y, ((max x y): ys))

splitMin' :: Ord a => [a] -> (a, [a])
splitMin' xs = (minimum' xs, filtrarElemento (minimum' xs) xs)

-- AGRUPAR -- 
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs) = agregarAGrupos x (agrupar xs)

agregarAGrupos :: Eq a => a -> [[a]] -> [[a]]
agregarAGrupos x [] = [[x]]
agregarAGrupos x (xs:xss) =
	if pertenece x xs
		then (x:xs):xss
		else [x]:xs:xss