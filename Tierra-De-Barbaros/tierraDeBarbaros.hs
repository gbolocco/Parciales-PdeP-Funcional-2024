import Data.Char (toUpper)

data Barbaro = Barbaro{
    nombre :: String,
    fuerza :: Int,
    habilidades :: [String],
    objetos :: [Objeto]
}

dave :: Barbaro
dave = Barbaro "Dave" 100 ["tejer","escribirPoesia", "Escribir Poesia Atroz"] [ardilla]

astro :: Barbaro
astro = Barbaro "Astro" 100 ["tejer", "robar","escribirPoesia", "Escribir Poesia Atroz"] [ardilla]

barbaro1 :: Barbaro
barbaro1 = Barbaro "Conan" 120 ["Luchar", "Correr"] []

barbaro2 :: Barbaro
barbaro2 = Barbaro "Thorgrim" 110 ["Gritar", "Beber"] []

barbaro3 :: Barbaro
barbaro3 = Barbaro "Red Sonja" 100 ["Esgrimir", "Navegar"] []

barbaro4 :: Barbaro
barbaro4 = Barbaro "Krom" 90 ["Construir", "Explorar"] []

barbaro5 :: Barbaro
barbaro5 = Barbaro "Valeria" 105 ["Nadar", "Cazar"] []

type Objeto = Barbaro -> Barbaro

-- 1

espada :: Int -> Objeto
espada peso unBarbaro = unBarbaro{fuerza = fuerza unBarbaro + 2 * peso}

amuletosMiticos :: String -> Objeto
amuletosMiticos = agregarHabilidad

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = agregarHabilidad "hacer magia" unBarbaro{objetos = []}

ardilla :: Objeto
ardilla = id

cuerda :: Objeto -> Objeto -> Objeto
cuerda primerObjeto segundoObjeto = primerObjeto . segundoObjeto

-- extraccion de logica
agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = unBarbaro{habilidades = unaHabilidad : habilidades unBarbaro}

-- 2

megafono :: Objeto
megafono unBarbaro = unBarbaro{habilidades = potenciarHabilidades (habilidades unBarbaro)}

potenciarHabilidades :: [String] -> [String]
potenciarHabilidades habilidades = [map toUpper (concat habilidades)]

megafonoBarbico :: Objeto
megafonoBarbico = cuerda ardilla megafono

-- 3
type Evento = Barbaro -> Bool

invasionDeDuendes :: Evento
invasionDeDuendes = poseeHabilidad "Escribir Poesia Atroz"

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = (||) (nombre unBarbaro == "Faffy") (nombre unBarbaro == "Astro")

type Prueba = Barbaro -> Bool -- parecido a evento

ritualDeFechorias :: Evento
ritualDeFechorias unBarbaro = any (pasaEvento unBarbaro) [saqueo, gritoDeGuerra, caligrafia]


pasaEvento :: Barbaro -> (Barbaro -> Bool) -> Bool
pasaEvento unBarbaro unEvento = unEvento unBarbaro

-- ==========

saqueo :: Prueba
saqueo unBarbaro = (&&) (poseeHabilidad "robar" unBarbaro) ((>80) . fuerza $ unBarbaro)

-- ==========

gritoDeGuerra :: Prueba
gritoDeGuerra unBarbaro = (== 4 * length (objetos unBarbaro)) . poderGritoDeGuerra $ unBarbaro

poderGritoDeGuerra :: Barbaro -> Int
poderGritoDeGuerra unBarbaro = sum . map length $ habilidades unBarbaro

-- ==========

caligrafia :: Prueba
caligrafia unBarbaro = all primeraLetraMayuscula (habilidades unBarbaro) && all poseeMasdeTresVocales (habilidades unBarbaro)

primeraLetraMayuscula :: String -> Bool
primeraLetraMayuscula palabra = head palabra == toUpper (head palabra)

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiou" || elem letra "AEIOU"

poseeMasdeTresVocales :: String -> Bool
poseeMasdeTresVocales = (>3) . length . filter esVocal


-- extraccion de logica
poseeHabilidad :: String -> Barbaro -> Bool
poseeHabilidad habilidad unBarbaro = elem habilidad (habilidades unBarbaro)

-- ==========
type Aventura = [Evento]

sobrevivientes :: Aventura -> [Barbaro] -> [Barbaro]
sobrevivientes unaAventura = filter (\barbaro -> all (pasaEvento barbaro) unaAventura)


escalarMontania :: Aventura
escalarMontania = [invasionDeDuendes, cremalleraDelTiempo, ritualDeFechorias]

laBanda :: [Barbaro]
laBanda = [dave, barbaro1, barbaro2, barbaro3, barbaro4, barbaro5, astro]

{-
sobrevivientes escalarMontania laBanda
Astro
-}

-- 4
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
    | elem x xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs

desendiente :: Barbaro -> [Barbaro]
desendiente = iterate crearDesendiente

crearDesendiente :: Barbaro -> Barbaro
crearDesendiente unBarbaro = aplicarObjetos (objetos unBarbaro) unBarbaro{nombre = nombre unBarbaro ++ "*", habilidades = sinRepetidos (habilidades unBarbaro)}

aplicarObjetos :: [Objeto] -> Barbaro -> Barbaro
aplicarObjetos unosObjetos unBarbaro = foldr ($) unBarbaro unosObjetos