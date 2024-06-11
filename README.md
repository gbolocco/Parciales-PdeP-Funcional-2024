# Parciales-PdeP-Funcional-2024

**Importar Funciones:**

*Funcion toUpper*
import Data.Char (toUpper)

*Funcion sort (ordena de forma ascendente)* 
import Data.List (sort)

**Funciones Clave:**

*Funcion para encontrar un elemento en una lista de tuplas*

[("nombre",elemento)]
encontrar :: String -> [lista de tuplas] -> elemento
encontrar elemento listaDeElementos = head . filter ((==elemento).fst) $ listaDeElementos

*Funcion que elimina los elementos repetidos dentro de una lista*

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
    | elem x xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

*Funcion que ordena una lista de elementos bajo un criterio especifico*

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

*Funcion para restriccion de valores*

inflingirDaño persona daño = persona{salud = max 0 (salud unPersonaje - daño)} --la salud no puede ser menor que 0

*Aplicar una lista de funciones a un elemento*

dichas funciones tienen que ir de tipoElemento -> tipoElemento

foldr ($) elemento [funciones]
foldl (flip $) elemento [funciones]

*Aplicar un ekemento a una lista de funciones, me devuelve una lista con las funciones aplicadas al elemento*

aplicar :: a -> [a -> a] -> [a]
aplicar elemento funciones = map ($ elemento) funciones