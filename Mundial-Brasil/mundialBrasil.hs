data Jugador = Jugador{
    nombreJugador :: String,
    edad :: Int,
    promedioGol :: Float,
    habilidad :: Int,
    cansancio :: Float
}deriving(Show)

data Equipo = Equipo{
    nombreEquipo :: String,
    jugadores :: [Jugador],
    grupo :: Char
}deriving(Show)

-- ==========

martin :: Jugador
martin = Jugador "Martin" 26 0.0 50 35.0

juan :: Jugador
juan = Jugador "Juancho" 30 0.2 50 40.0

maxi :: Jugador
maxi = Jugador "Maxi Lopez" 27 0.4 68 30.0

jonathan :: Jugador
jonathan = Jugador "Chueco" 20 1.5 80 99.0

lean :: Jugador
lean = Jugador "Hacha" 23 0.01 50 35.0

brian :: Jugador
brian = Jugador "Panadero" 21 5 80 15.0

garcia :: Jugador
garcia = Jugador "Sargento" 30 1 80 13.0

messi :: Jugador
messi = Jugador "Pulga" 26 10 99 43.0

aguero :: Jugador
aguero = Jugador "Aguero" 24 5 90 5.0

balanza :: Equipo
balanza = Equipo{
    nombreEquipo = "Lo Que Vale Es El Intento",
    jugadores = [martin, jonathan, maxi],
    grupo = 'F'
}

restoDelMundo :: Equipo
restoDelMundo = Equipo{
    nombreEquipo = "Resto del Mundo",
    jugadores = [juan, messi, aguero],
    grupo = 'A'
}

losDeSiempre :: Equipo
losDeSiempre = Equipo{
    nombreEquipo = "Los De Siempre",
    jugadores = [garcia, lean, brian],
    grupo = 'F'
}

-- ==========

-- 1
figuras :: Equipo -> [Jugador]
figuras = filter esFigura.jugadores

esFigura :: Jugador -> Bool
esFigura unJugador = ((>75).habilidad $ unJugador) && ((>0).promedioGol $ unJugador)

-- 2

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFaranduleros :: Equipo -> Bool
tieneFaranduleros unEquipo = any esFarandulero (jugadores unEquipo)

esFarandulero :: Jugador -> Bool
esFarandulero unJugador = elem (nombreJugador unJugador) jugadoresFaranduleros

-- 3

figuritasDificiles :: [Equipo] -> Char -> [String]
figuritasDificiles unosEquipos unGrupo = map nombreJugador (concatMap (filter esDificil . jugadores) (filter ((==unGrupo).grupo) unosEquipos))

esDificil :: Jugador -> Bool
esDificil unJugador = esFigura unJugador && esJoven unJugador && (not.esFarandulero $ unJugador)

esJoven :: Jugador -> Bool
esJoven = (<27) . edad

-- 4
jugarPartido :: Equipo -> Equipo
jugarPartido unEquipo = unEquipo{jugadores = map afectarJugador (jugadores unEquipo) }

afectarJugador :: Jugador -> Jugador
afectarJugador unJugador
    | esDificil unJugador = afectarCansancio unJugador (- cansancio unJugador + 50)
    | esJoven unJugador = afectarCansancio unJugador (cansancio unJugador * 0.1)
    | (not . esJoven) unJugador && esFigura unJugador = afectarCansancio unJugador 20
    | otherwise = afectarCansancio unJugador (cansancio unJugador)

afectarCansancio :: Jugador -> Float -> Jugador
afectarCansancio unJugador valor = unJugador{cansancio = cansancio unJugador + valor}

-- 5
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

type Criterio = Jugador -> Jugador -> Bool

criterioCansancio :: Criterio
criterioCansancio jugador1 jugador2 = cansancio jugador1 <= cansancio jugador2

definirGanadorEntre :: Equipo -> Equipo -> Equipo
definirGanadorEntre equipo1 equipo2
    | (promedioGolTotal . equipoTitular $ equipo1) > (promedioGolTotal . equipoTitular $ equipo2) = jugarPartido equipo1
    | otherwise = jugarPartido equipo2

equipoTitular :: Equipo -> [Jugador]
equipoTitular unEquipo = take 11 (quickSort criterioCansancio (jugadores unEquipo))

promedioGolTotal :: [Jugador] -> Float
promedioGolTotal =  sum . map promedioGol

-- 6
    -- opcion 1:
ganadorDelTorneo :: [Equipo] -> Equipo
ganadorDelTorneo [equipo] = equipo
ganadorDelTorneo (e:es) = foldr definirGanadorEntre e (e:es)

    -- opcion 2:
ganadorDelTorneo' :: [Equipo] -> Equipo
ganadorDelTorneo' [equipo] = equipo
ganadorDelTorneo' (equipo1 : equipo2 : equipos) = ganadorDelTorneo' . (:equipos) $ definirGanadorEntre equipo1 equipo2

-- 7

definirElGroso :: [Equipo] -> Jugador
definirElGroso unosEquipos =   modificarNombre (head . filter esFigura . jugadores . ganadorDelTorneo $ unosEquipos) " El Groso"

modificarNombre :: Jugador -> String -> Jugador
modificarNombre unJugador unApodo = unJugador{nombreJugador = nombreJugador unJugador ++ unApodo}