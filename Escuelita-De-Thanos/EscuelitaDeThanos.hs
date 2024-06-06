data Guante = Guante {
    material :: String,
    gemas :: [Gema]
} 

data Personaje = Personaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving(Show,Eq)

type Universo = [Personaje]

guanteDeThanos :: Guante
guanteDeThanos = Guante {
    material = "uru",
    gemas = []
}

-- =====[Personajes]=====
ironMan :: Personaje
ironMan = Personaje {
    edad = 48,
    energia = 80.0,
    habilidades = ["Ingeniería", "Combate cuerpo a cuerpo", "Armadura avanzada"],
    nombre = "Tony Stark",
    planeta = "Tierra"
}

captainAmerica :: Personaje
captainAmerica = Personaje {
    edad = 100,  -- Edad real incluyendo el tiempo en congelación
    energia = 70.0,
    habilidades = ["Super fuerza", "Agilidad", "Uso del escudo"],
    nombre = "Steve Rogers",
    planeta = "Tierra"
}

thor :: Personaje
thor = Personaje {
    edad = 1500,
    energia = 90.0,
    habilidades = ["Control del trueno", "Super fuerza", "Vuelo", "Uso del Mjolnir"],
    nombre = "Thor",
    planeta = "Asgard"
}

blackWidow :: Personaje
blackWidow = Personaje {
    edad = 35,
    energia = 60.0,
    habilidades = ["Artes marciales", "Espionaje", "Uso de armas"],
    nombre = "Natasha Romanoff",
    planeta = "Tierra"
}

hulk :: Personaje
hulk = Personaje {
    edad = 45,
    energia = 100.0,
    habilidades = ["Super fuerza", "Regeneración", "Invulnerabilidad"],
    nombre = "Bruce Banner",
    planeta = "Tierra"
}

spiderMan :: Personaje
spiderMan = Personaje {
    edad = 22,
    energia = 65.0,
    habilidades = ["Trepar paredes", "Sentido arácnido", "Super agilidad", "Lanzar telarañas"],
    nombre = "Peter Parker",
    planeta = "Tierra"
}

scarletWitch :: Personaje
scarletWitch = Personaje {
    edad = 29,
    energia = 90.0,
    habilidades = ["Magia del caos"],
    nombre = "Wanda Maximoff",
    planeta = "Tierra"
}
-- ==========

marvel :: Universo
marvel = [scarletWitch, spiderMan, hulk, blackWidow, thor, captainAmerica, ironMan]

-- Guantelete completo: 6 gemas y material "uru"
completo :: Guante -> Bool
completo guante = ((==6).length.gemas $ guante) && (material guante == "uru")

chasquido :: Universo -> Guante -> [String]
chasquido universo guante
    | completo guante = map nombre (drop (div (length universo) 2) universo)
    | otherwise = map nombre universo

-- Pendex
pendex :: Universo -> Bool
pendex universo = any (<45) (map edad universo)

-- Energia Total
sumatoriaDeEnergias :: Universo -> Float
sumatoriaDeEnergias unUniverso = foldr (\acum x -> acum + x) 0 (map energia unUniverso)

energiaTotal :: Universo -> Float
energiaTotal unUniverso = sumatoriaDeEnergias (filter ((>1).length.habilidades) unUniverso)

-- =====[Parte 2]=====
type Gema = Personaje -> Personaje

{-
    ● La mente que tiene la habilidad de debilitar la energía de un usuario en un valor
dado.
    ● El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una
habilidad en particular si es que la posee. Además le quita 10 puntos de energía.

Paradigmas de Programación
    ● El espacio que permite transportar al rival al planeta x (el que usted decida) y resta
20 puntos de energía.
    ● El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en
caso contrario no le saca ninguna habilidad).
    ● El tiempo que reduce a la mitad la edad de su oponente pero como no está
permitido pelear con menores, no puede dejar la edad del oponente con menos de
18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le
quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le
deben quedar 18 en lugar de 15. También resta 50 puntos de energía.
    ● La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces
contra un rival.

Punto 3: (3 puntos) Implementar las gemas del infinito, evitando lógica duplicada.
-}

laMente :: Float -> Gema
laMente valor unPersonaje = unPersonaje { energia = reducirEnergia (energia unPersonaje * (valor/100)) unPersonaje }

elAlma :: String -> Gema
elAlma unaHabilidad unPersonaje = unPersonaje { habilidades = filter (/= unaHabilidad) (habilidades unPersonaje) , energia = reducirEnergia 10 unPersonaje }

elEspacio :: String -> Gema
elEspacio unPlaneta unPersonaje = unPersonaje { planeta = unPlaneta, energia = reducirEnergia 20 unPersonaje }

elPoder :: Gema
elPoder unPersonaje
    | ((<=2).length.habilidades $ unPersonaje) = unPersonaje { habilidades = [], energia = reducirEnergia (energia unPersonaje) unPersonaje }
    | otherwise = unPersonaje { energia = reducirEnergia (energia unPersonaje) unPersonaje }

elTiempo :: Gema
elTiempo unPersonaje
    | (div (edad unPersonaje) 2) > 18 = unPersonaje { edad = div (edad unPersonaje) 2, energia = reducirEnergia 50 unPersonaje} 
    | otherwise = unPersonaje { edad = 18, energia = reducirEnergia 50 unPersonaje }

gemaLoca :: Gema -> Gema 

gemaLoca unaGema unPersonaje = unaGema.unaGema $ unPersonaje

-- extraccion de logica
reducirEnergia :: Float -> Personaje -> Float 
reducirEnergia valor unPersonaje = energia unPersonaje - valor

guanteleteDeGoma :: Guante
guanteleteDeGoma = Guante {
    material = "goma",
    gemas = [elTiempo, elAlma "usar Mjolnir", gemaLoca (elAlma "programacion enHaskell")]

}

--Punto 5
usarGemas :: [Gema] -> Personaje -> Personaje
usarGemas unasGemas unPersonaje = foldr ($) unPersonaje unasGemas

{- Ejemplo de uso
ghci> spiderMan
Personaje {edad = 22, energia = 65.0, habilidades = ["Trepar paredes","Sentido ar\225cnido","Super agilidad","Lanzar telara\241as"], nombre = "Peter Parker", planeta = "Tierra"}

ghci> usarGemas [elTiempo, elAlma "Super agilidad", elPoder, elEspacio "Neptuno"] spiderMan
Personaje {edad = 18, energia = -60.0, habilidades = ["Trepar paredes","Sentido ar\225cnido","Lanzar telara\241as"], nombre = "Peter Parker", planeta = "Neptuno"}
-}

guanteleteDePruebas :: Guante
guanteleteDePruebas = Guante {
    material = "uru",
    gemas = [elTiempo, elAlma "Super agilidad", elPoder, elEspacio "Neptuno"]
}

--Punto 6
-- Función recursiva para encontrar la gema más poderosa  !! ANALIZAR FUNCION
gemaMasPoderosaAux :: [Gema] -> Personaje -> Gema -> Gema
gemaMasPoderosaAux [] _ gemaMasPoderosaActual = gemaMasPoderosaActual
gemaMasPoderosaAux (g:gs) unPersonaje gemaMasPoderosaActual
    | energia (g unPersonaje) < energia (gemaMasPoderosaActual unPersonaje) = gemaMasPoderosaAux gs unPersonaje g
    | otherwise = gemaMasPoderosaAux gs unPersonaje gemaMasPoderosaActual

-- Función principal
gemaMasPoderosa :: Guante -> Personaje -> Gema
gemaMasPoderosa unGuante _ = error "El guantelete no tiene gemas"
gemaMasPoderosa unGuante unPersonaje = gemaMasPoderosaAux (gemas unGuante) unPersonaje (head (gemas unGuante))

-- 7
{-
Realizar
-}