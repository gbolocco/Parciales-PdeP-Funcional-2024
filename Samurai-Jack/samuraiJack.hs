data Elemento = Elemento {
    tipo :: String,
    ataque :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje) 
}

data Personaje = Personaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
}

-- personajes
aku :: Personaje
aku = Personaje {
    nombre = "Aku",
    salud = 100,
    elementos = [espadaMagica, espadaMaligna],
    anioPresente = 2024
}

-- elementos

espadaMaligna :: Elemento
espadaMaligna = Elemento {
    tipo = "maldad",
    ataque = (causarDanio 20),
    defensa = meditar
}
espadaMagica :: Elemento
espadaMagica = Elemento {
    tipo = "acero",
    ataque = (causarDanio 10),
    defensa = meditar
}



----------------------------------------

{-
1. Empecemos por algunas transformaciones básicas:
    a. mandarAlAnio: lleva al personaje al año indicado.
    b. meditar: le agrega la mitad del valor que tiene a la salud del personaje.
    c. causarDanio: le baja a un personaje una cantidad de salud dada.
    Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede
    quedar menor a 0.
Importante: no repetir lógica.
-}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje {anioPresente = unAnio}

meditar :: Personaje -> Personaje
meditar unPersonaje = modificarSalud (salud unPersonaje / 2) unPersonaje

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio unPersonaje = modificarSalud (max 0 (salud unPersonaje - danio)) unPersonaje

modificarSalud :: Float -> Personaje -> Personaje
modificarSalud valor unPersonaje = unPersonaje { salud = salud unPersonaje + valor }

----------------------------------------

{-
2. Queremos poder obtener algo de información extra sobre los personajes. Definir las
siguientes funciones:
    a. esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje
en cuestión es de tipo “Maldad”.
    b. danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre
la salud inicial del personaje y la salud del personaje luego de usar el ataque del
elemento sobre él.
    c. enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista
de los enemigos que pueden llegar a matarlo con un solo elemento. Esto sucede si luego de
aplicar el efecto de ataque del elemento, el personaje queda con salud igual a 0.
-}

esMalvado :: Personaje -> Bool
esMalvado unPersonaje = any (== "maldad") (map tipo (elementos aku))

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = salud unPersonaje - (salud.(ataque unElemento)  $ unPersonaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje enemigos = filter (poseeUnElementoMortal unPersonaje) enemigos

poseeUnElementoMortal :: Personaje -> Personaje -> Bool
                                                         --salud == 0 ?       aplica el personaje a la lista de ataques de los elementos del enemigo
poseeUnElementoMortal unPersonaje unEnemigo =  any (==0) (map salud (map ($ unPersonaje) (map ataque (elementos unEnemigo))))

{-
3. Definir los siguientes personajes y elementos:
    a.  Definir concentracion de modo que se pueda obtener un elemento cuyo efecto
        defensivo sea aplicar meditar tantas veces como el nivel de concentración indicado y
        cuyo tipo sea "Magia".
    b.  Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad
        de esbirros (que son elementos de tipo “Maldad” cuyo efecto ofensivo es causar un
        punto de daño).
    c.  Definir jack de modo que permita obtener un personaje que tiene 300 de salud, que
        tiene como elementos concentración nivel 3 y una katana mágica (de tipo "Magia" cuyo
        efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.
    d.  Definir aku :: Int -> Float -> Personaje que recibe el año en el que vive y la cantidad
        de salud con la que debe ser construido. Los elementos que tiene dependerán en parte
        de dicho año. Los mismos incluyen:
            i. Concentración nivel 4
            ii. Tantos esbirros malvados como 100 veces el año en el que se encuentra.
            iii. Un portal al futuro, de tipo “Magia” cuyo ataque es enviar al personaje al futuro
                (donde el futuro es 2800 años después del año indicado para aku), y su defensa
                genera un nuevo aku para el año futuro correspondiente que mantenga la salud
                que tenga el personaje al usar el portal.
-}

concentracion :: Int -> Elemento
concentracion nivel = Elemento {
    tipo = "magia",
    ataque = (causarDanio 1000),
    defensa = meditar
}