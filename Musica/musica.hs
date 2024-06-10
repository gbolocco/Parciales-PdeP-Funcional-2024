type Cancion = [Nota]

-- ==========

cancionEjemplo :: Cancion
cancionEjemplo = [
    Nota 10.0 60.0 1.0,
    Nota 309.3 5 0.5,
    Nota 30049.2 55.0 2.0,
    Nota 261.6 8.0 1.5,
    Nota 293.7 50.0 0.75,
    Nota 329.6 90.0 1.25 
    ]
-- ==========

data Nota = Nota {
    tono :: Float, -- Frecuencia medida en Hz
    volumen :: Float, -- Volumen de reproducción medido en Db
    duracion :: Float -- Tiempo de reproducción medido en segundos
} deriving (Eq, Show)

-- FUNCIONES AUXILIARES
cambiarVolumen :: (Float -> Float) -> Nota -> Nota

-- Dada una función transformación y una nota retorna una copia de la
-- nota con el volumen igual al resultado de aplicar la transformación a
-- su volumen actual.

cambiarVolumen delta nota = nota { volumen = delta (volumen nota) }

cambiarTono :: (Float -> Float) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el tono igual al resultado de aplicar la transformación a
-- su tono actual.
cambiarTono delta nota = nota { tono = delta (tono nota) }

cambiarDuracion :: (Float -> Float) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con la duración igual al resultado de aplicar la transformación a
-- su duración actual.
cambiarDuracion delta nota = nota { duracion = delta (duracion nota) }

promedio :: [Float] -> Float
-- Dada una lista de números retorna el valor promedio
promedio lista = sum lista / fromIntegral (length lista)

-- 1
    --  A
esAudible :: Nota -> Bool
esAudible unaNota = tonoAudible unaNota && volumenAudible unaNota

volumenAudible :: Nota -> Bool
volumenAudible = (>10).volumen

tonoAudible :: Nota -> Bool
tonoAudible unaNota = ((>20) . tono $ unaNota) && ((<20000) . tono $ unaNota)

    -- B
esMolesta :: Nota -> Bool

esMolesta unaNota
    | esAudible unaNota && ((<250) . tono $ unaNota) = (>85) . volumen $ unaNota --True
    | esAudible unaNota && ((>=259) . tono $ unaNota) = (>55) . volumen $ unaNota --True
    | otherwise = esAudible unaNota --False

-- 2

silencioTotal :: Cancion -> Float
silencioTotal unaCancion = sum (map duracion . filter (not.esAudible) $ unaCancion)

sinInterrupciones :: Cancion -> Bool
sinInterrupciones unaCancion = all esAudible (filter ((>0.5).duracion) unaCancion)

peorMomento :: Cancion -> Float
peorMomento unaCancion = maximum (map duracion . filter esMolesta $ unaCancion)

-- 3

type Filtro = Cancion -> Cancion --ajustar las notas de acuerdo a un criterio

    -- a
trasponer :: Float -> Filtro
trasponer escalar = map (cambiarTono (*escalar))

    -- b
acotarVolumen :: Float -> Float -> Filtro
acotarVolumen _ _ [] = []
acotarVolumen volMax volMin (x:xs)
    | estaEntreVolumenes volMax volMin x = x : acotarVolumen volMax volMin xs
    | volumenAlto volMax x = cambiarVolumen (min volMax) x : acotarVolumen volMax volMin xs
    | volumenBajo volMin x = cambiarVolumen (max volMin) x : acotarVolumen volMax volMin xs

estaEntreVolumenes :: Float -> Float -> Nota -> Bool
estaEntreVolumenes max min unaNota = volumenBajo min unaNota && volumenAlto max unaNota

volumenAlto :: Float -> Nota -> Bool
volumenAlto max = (>=max) . volumen

volumenBajo :: Float -> Nota -> Bool
volumenBajo min = (<=min) . volumen

    -- c
normalizar :: Filtro
normalizar unaCancion = map (cambiarVolumen (const (promedio (map volumen unaCancion)))) unaCancion

-- 4
    -- b
infringeCopyright :: [Filtro] -> Cancion -> Cancion -> Bool
infringeCopyright filtros unaCancion otraCancion =  any ((== unaCancion) . ($ otraCancion)) filtros || unaCancion == otraCancion

-- 5
tunear :: Cancion -> [Filtro] -> Cancion
tunear unaCancion = normalizar . foldr ($) unaCancion