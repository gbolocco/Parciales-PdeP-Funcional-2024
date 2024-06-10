# Parciales-PdeP-Funcional-2024

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