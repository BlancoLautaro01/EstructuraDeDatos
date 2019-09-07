import Practica_1 
-- PRACTICA 2 --
----------------
-----------------------
-- TIPOS ALGEBRAICOS --
-----------------------
-------
-- 1 --
-------
data Dir = Norte 
		| Sur
		| Este
		| Oeste
		deriving (Show,Eq)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este
opuesto Este = Oeste

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Sur = Oeste
siguiente Oeste = Norte
siguiente Este = Sur

-------
-- 2 --
-------
-------------------------
-- Definicion de Tipos --
-------------------------
data Persona = Pers String Int
			deriving (Show, Eq)

----------------
-- Instancias --
----------------
jorge = (Pers "Jorge" 22)
maria = (Pers "Maria" 42)
---------------------
-- GETTERS PERSONA --
---------------------
nombrePersona :: Persona -> String
nombrePersona (Pers x y) = x

edad :: Persona -> Int
edad (Pers x y) = y

---------------
-- Funciones --
---------------
-------
-- A --
-------
crecer :: Persona -> Persona
crecer (Pers x y) = (Pers x (y+1))

-------
-- B --
-------
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoN (Pers x y) = (Pers nuevoN y)

-------
-- C --
-------
esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (Pers x1 y1) (Pers x2 y2) = y1 < y2

-------
-- D --
-------
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) = 
	if (edad x >= n)
		then  (x: mayoresA n xs)
		else mayoresA n xs

-------
-- E --
-------
promedioEdad :: [Persona] -> Int
-- La lista posee al menos una Persona.
promedioEdad xs = div (sumatoria (mapeoEdades xs)) (longitud xs)

mapeoEdades :: [Persona] -> [Int]
mapeoEdades [] = []
mapeoEdades (x:xs) = (edad x: mapeoEdades xs)

-------
-- F --
-------
elMasViejo :: [Persona] -> Persona
-- La lista posee al menos una Persona.
elMasViejo [x] = x
elMasViejo (x:xs) =
	if (edad x > edad (elMasViejo xs))
		then x
		else elMasViejo xs

-------
-- 3 --
-------
------------------------
--Definicion de Tipos --
------------------------
data Pokemon = Agua Int
			| Fuego Int
			| Planta Int
			deriving (Show, Eq)

data Entrenador = Ent String [Pokemon]
				deriving (Show, Eq)

----------------
-- Instancias --
----------------
squirtle = Agua 20
bulvasaur = Planta 50
charmander = Fuego 30
ash = Ent "Ash" [charmander]

---------------------
-- GETTERS POKEMON --
---------------------
tipoDelPokemon :: Pokemon -> String
tipoDelPokemon (Agua n) = "Agua"
tipoDelPokemon (Fuego n) = "Fuego"
tipoDelPokemon (Planta n) = "Planta"

energia :: Pokemon -> Int
energia (Fuego n) = n
energia (Agua n) = n
energia (Planta n) = n

------------------------
-- GETTERS ENTRENADOR --
------------------------

nombreEntrenador :: Entrenador -> String
nombreEntrenador (Ent x xs) = x

listaPokemons :: Entrenador -> [Pokemon]
listaPokemons (Ent x xs) = xs

---------------
-- Funciones --
---------------
-------
-- A --
-------
leGanaAlToque :: Pokemon -> Pokemon -> Bool
leGanaAlToque (Agua n1) (Fuego n2) = True
leGanaAlToque (Fuego n1) (Planta n2) = True
leGanaAlToque (Planta n1) (Agua n2) = True
leGanaAlToque p1 p2 =
	error "Los tipos no son suficientes para saber quien gana"

-------
-- B --
-------
capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon x (Ent n xs) = Ent n (x:xs)

-------
-- C --
-------
cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons (Ent n xs) = longitud xs

-------
-- D --
-------
cantidadPokemonsDeTipo :: Pokemon -> Entrenador -> Int
cantidadPokemonsDeTipo x (Ent n xs) = longitud (filtroTipos xs x)

filtroTipos :: [Pokemon] -> Pokemon -> [Pokemon]
filtroTipos [] y = []
filtroTipos (x:xs) y = 
	if (tipoDelPokemon x == tipoDelPokemon y)
		then (x:filtroTipos xs y)
		else (filtroTipos xs y)

-------
-- E --
-------
esExperto :: Entrenador -> Bool
esExperto (Ent x [p1, p2]) = False
esExperto x = 
	((cantidadPokemonsDeTipo (Agua 1) x) >= 1)
	&& ((cantidadPokemonsDeTipo (Fuego 1) x) >= 1)
	&& ((cantidadPokemonsDeTipo (Planta 1) x) >= 1)

-------
-- 4 --
-------
-------------------------
-- Definicion de Tipos --
-------------------------
data Pizza = Prepizza
			| Agregar Ingrediente Pizza
			deriving (Show, Eq)

data Ingrediente = Salsa
				| Queso
				| Jamon
				| AceitunasVerdes Int
				deriving (Show, Eq)

----------------
-- Instancias --
----------------
jamonYQueso = 
	Agregar (AceitunasVerdes 8) (Agregar Jamon (Agregar Queso (Agregar Salsa Prepizza)))
quesoYSalsa = 
	Agregar (AceitunasVerdes 4) (Agregar Queso (Agregar Salsa Prepizza))
quesoDobleAceituna = 
	Agregar (AceitunasVerdes 8) (Agregar Queso (Agregar Salsa (Agregar (AceitunasVerdes 8) Prepizza)))
prepizza1 = 
	Prepizza

---------------
-- Funciones --
---------------
-------
-- A --
-------
ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Agregar ingrediente pizza) = (ingrediente:ingredientes pizza)

-------
-- B --
-------
tieneJamon :: Pizza -> Bool
tieneJamon x = pertenece Jamon (ingredientes x)

-------
-- D --
-------
-- Recorre una lista de ingredientes x concatenandolos con campo Agregar x Pizza \ Prepizza.
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = (Agregar x (armarPizza xs))

-------
-- C --
-------
sacarJamon :: Pizza -> Pizza
sacarJamon x = sacarIngrediente Jamon x

sacarIngrediente :: Ingrediente -> Pizza -> Pizza
{- Toma la lista de ingredientes de Pizza, se le saca
 el ingrediente y, y se arma una nueva Pizza. -}
sacarIngrediente y x = armarPizza (listaSinIngrediente y (ingredientes x))

listaSinIngrediente :: Ingrediente -> [Ingrediente] -> [Ingrediente]
{-Genera una lista de ingredientes nueva sacando todas 
-- las apariciones del que se le pasa por parametro. -}
listaSinIngrediente x xs = (filtrarElemento x xs)

-------
-- E --
-------
duplicarAceitunas :: Pizza -> Pizza
{- Toma la lista de ingredientes, le duplica los int a las aceitunas
 y genera una nueva pizza con esa lista nueva -}
duplicarAceitunas x = armarPizza (buscarAceitunasYDuplicarlas (ingredientes x))

buscarAceitunasYDuplicarlas :: [Ingrediente] -> [Ingrediente]
{- Dada una lista de ingredientes, chequea que sean AceitunasVerdes
 y de serlo, les duplica sun Int -}
buscarAceitunasYDuplicarlas [] = []
buscarAceitunasYDuplicarlas (x:xs) =
	if (esAceituna x)
		then (AceitunasVerdes ((nAceitunas x)*2): buscarAceitunasYDuplicarlas xs)
		else (x: buscarAceitunasYDuplicarlas xs)

esAceituna :: Ingrediente -> Bool
esAceituna (AceitunasVerdes n) = True
esAceituna Queso = False
esAceituna Salsa = False
esAceituna Jamon = False

nAceitunas :: Ingrediente -> Int
nAceitunas (AceitunasVerdes n) = n

-------
-- F --
-------
sacar :: [Ingrediente] -> Pizza -> Pizza
sacar (x:xs) y = sacarIngrediente x (sacar xs y)

-------
-- G --
-------
cantJamon :: [Pizza] -> [(Int, Pizza)]
cantJamon [] = []
cantJamon (x:xs) = (((apariciones Jamon (ingredientes x)), x): cantJamon xs)

-------
-- H --
-------
mayorNAceitunas :: Int -> [Pizza] -> [Pizza]
mayorNAceitunas n [] = []
mayorNAceitunas n (x:xs) = 
	if ((nAceitunasPorPizza x) > n)
		then (x: mayorNAceitunas n xs)
		else mayorNAceitunas n xs

nAceitunasPorPizza :: Pizza -> Int
nAceitunasPorPizza x = totalAceitunas (ingredientes x)

totalAceitunas :: [Ingrediente] -> Int
totalAceitunas [] = 0
totalAceitunas (x:xs) = 
	if (esAceituna x)
		then sumar (nAceitunas x) (totalAceitunas xs)
		else totalAceitunas xs

-------
-- 5 --
-------
-------------------------
-- Definicion de Tipos --
-------------------------
data Objeto = Cacharro
			| Tesoro
			deriving (Show, Eq)

data Camino = Fin
			| Cofre [Objeto] Camino
			| Nada Camino
			deriving (Show, Eq)

----------------
-- Instancias --
----------------

objeto1 = Cacharro
objeto2 = Tesoro
camino1 = Fin
camino2 = Nada (Cofre [objeto2] (Nada (Cofre [objeto1, objeto2] (Nada (Cofre [objeto1, objeto2] (Cofre [] (Fin)))))))

---------------
-- Funciones --
---------------
-------
-- A --
-------
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada x) = hayTesoro x
hayTesoro (Cofre xs y) = 
	if (pertenece Tesoro (xs))
		then True
		else (hayTesoro y)

-------
-- B --
-------
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada camino) = (1 + (pasosHastaTesoro camino))
pasosHastaTesoro (Cofre xs camino) = 
	if pertenece Tesoro xs
		then 0
		else 1 + pasosHastaTesoro camino

-------
-- C --
-------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn 0 (Nada camino) = False
hayTesoroEn n (Nada camino) = (hayTesoroEn (n-1) camino)
hayTesoroEn 0 (Cofre xs camino) = pertenece Tesoro xs
hayTesoroEn n (Cofre xs camino) = hayTesoroEn (n-1) camino

-------
-- D --
-------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = False
alMenosNTesoros n (Nada camino) = alMenosNTesoros n camino
alMenosNTesoros 1 (Cofre xs camino) = (pertenece Tesoro xs) || (alMenosNTesoros 1 camino)
alMenosNTesoros n (Cofre xs camino) = 
	if pertenece Tesoro xs
		then alMenosNTesoros (n-1) camino
		else alMenosNTesoros n camino

-------
-- E --
-------
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n n2 camino =
	if (n <= n2)
		then
			if (hayTesoroEn n camino)
				then 1 + cantTesorosEntre (n + 1) n2 camino
				else cantTesorosEntre (n + 1) n2 camino
		else 0
-- Nada (Cofre [objeto2] (Nada (Cofre [objeto1, objeto2] (Nada (Cofre [objeto1, objeto2] (Cofre [] (Fin)))))))