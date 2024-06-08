data Persona = Persona{
    nombre :: String,
    saldo :: Float,
    suerte :: Int,
    factores :: [Factor]
}deriving(Show)

data Juego = Juego{
    nombreJuego :: String,
    cuantoGanaCon :: Float -> Float,
    criterios :: [Criterio]
}

type Factor = (String,Int)

nico :: Persona
nico = Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)]
maiu :: Persona
maiu = Persona "Maiu" 100.0 97 [("inteligencia",55), ("paciencia",50)]
pedro :: Persona
pedro = Persona "pedro" 100.0 12 []
fran :: Persona
fran = Persona "fran" 100.0 12 []
-- 1

--suerteTotal :: Persona -> Int
--suerteTotal unaPersona
--    | null.factores $ unaPersona = suerte unaPersona
--    | otherwise = foldr ((*) . snd) (suerte unaPersona) (factores unaPersona)

suerteTotal :: Persona -> Int
suerteTotal unaPersona
    | elem "amuleto" (map fst (factores unaPersona)) = (suerte unaPersona *).snd $ encontrar "amuleto" (factores unaPersona)
    | otherwise = suerte unaPersona

encontrar :: String -> [Factor] -> Factor
encontrar elemento = head . filter ((==elemento).fst)  --FUNCION PARA ENCONTRAR UN VALOR EN UNA LISTA CLAVEEEE

-- 2

type Criterio = Persona -> Bool

ruleta :: Juego
ruleta = Juego {
    nombreJuego = "ruleta",
    cuantoGanaCon = cobrarApuestaRuleta,
    criterios = [criterioSuerte 80]
}

maquinita :: Float -> Juego
maquinita jackPot = Juego {
    nombreJuego = "maquinita",
    cuantoGanaCon = cobrarApuestaMaquinita jackPot,
    criterios = [criterioSuerte 95, criterioPoseeFactor "paciencia"]
}

-- funciones para extraer logica repetida

criterioPoseeFactor :: String -> Criterio
criterioPoseeFactor unFactor unaPersona = elem unFactor (map fst (factores unaPersona))

cobrarApuestaRuleta :: Float -> Float
cobrarApuestaRuleta apuesta = 37 * apuesta

cobrarApuestaMaquinita :: Float -> Float -> Float
cobrarApuestaMaquinita jackPot apuesta = jackPot + apuesta

criterioSuerte :: Int -> Criterio
criterioSuerte cantidadDeSuerte = (>cantidadDeSuerte) . suerteTotal

-- 3

gana :: Juego -> Persona -> Bool
gana unJuego unaPersona  = all ($ unaPersona) (criterios unJuego)

-- 4

-- A
juegosQueGana :: Persona -> [Juego] -> [Juego]
juegosQueGana unaPersona = filter (flip gana unaPersona)

cuantoDineroGanaria :: Persona -> Float -> [Juego] -> Float
cuantoDineroGanaria unaPersona unaApuesta =  foldr cuantoGanaCon unaApuesta . juegosQueGana unaPersona --ayudin de hoogle
--cuantoDineroGanaria unaPersona apuestaInicial =  foldr (\juego apuesta -> (cuantoGanaCon juego) apuesta) apuestaInicial . juegosQueGana unaPersona

-- B
cuantoDineroGanaria' :: Persona -> Float -> [Juego] -> Float
cuantoDineroGanaria' _ unaApuesta [] = 0
cuantoDineroGanaria' unaPersona unaApuesta (j:js)
    | gana j unaPersona = cuantoGanaCon j unaApuesta + cuantoDineroGanaria' unaPersona unaApuesta js
    | otherwise = cuantoDineroGanaria' unaPersona unaApuesta js

-- 5
noPuedenGanar :: [Persona] -> [Juego] -> [Persona]
noPuedenGanar (jug:jugs) juegos
    | all (\juego -> not (gana juego jug)) juegos = jug : noPuedenGanar jugs juegos
    | otherwise = noPuedenGanar jugs juegos

-- 6 
{-
6. Hacer que un jugador apueste una cantidad en un juego, que implica que la persona baje su saldo
esa cantidad y luego juegue al juego. Si puede ganar en ese juego retorna a la persona con su saldo
incrementado en lo que gana en el juego, de lo contrario retorna a la persona sin ganar nada.
-}

jugar :: Persona -> Float -> Juego -> Persona
jugar  unaPersona unaApuesta unJuego
    | saldo unaPersona < unaApuesta = unaPersona -- saldo Insuficiente paraApostar
    | gana unJuego unaPersona = modificarSaldo unaPersona (cuantoGanaCon unJuego unaApuesta) -- gana el juego
    | otherwise = modificarSaldo unaPersona ( - unaApuesta) -- pierde el juego

    
modificarSaldo :: Persona -> Float -> Persona
modificarSaldo unaPersona valor = unaPersona{saldo = saldo unaPersona + valor}