import Data.List (sort)

-- 1

type Cliente = (String,String)

data Chofer = Chofer{
    nombre :: String,
    kilometrake :: Int,
    viajes :: [Viaje],
    condicionAceptacion :: Condicion
}

data Viaje = Viaje{
    fecha :: Int,
    cliente :: Cliente, -- (Nombre, Direccion)
    costo :: Float
}deriving(Show)

-- 2

type Condicion = Viaje -> Bool

noCondicion :: Condicion
noCondicion = const True

condicionPrecio :: Condicion
condicionPrecio = (>200) . costo

condicionCantidadLetras :: Int -> Condicion
condicionCantidadLetras n = (>=n) . length . fst . cliente 

condicionZona :: String -> Condicion
condicionZona zona = (zona /=) . snd . cliente

-- 3

lucas :: Cliente
lucas = ("Lucas","Victoria")

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje 20042017 lucas 150] (condicionZona "Olivos")

alejandra :: Chofer
alejandra = Chofer  "Alejandra" 180000 [] noCondicion

-- 4 

aceptaViaje :: Chofer -> Viaje -> Bool
aceptaViaje = condicionAceptacion

-- 5

liquidacion :: Chofer -> Float
liquidacion unChofer = sum (map costo (viajes unChofer))

-- 6

    -- a

choferesQueAceptan :: [Chofer] -> Viaje -> [Chofer]
choferesQueAceptan choferes unViaje = filter (flip aceptaViaje unViaje) choferes

    -- b

menosViajes :: [Chofer] -> Chofer
menosViajes [unChofer] = unChofer
menosViajes (c1:c2:cs)
    | cantidadDeViajes c1 < cantidadDeViajes c2 = menosViajes (c1:cs)
    | otherwise = menosViajes (c2:cs)
    
cantidadDeViajes :: Chofer -> Int
cantidadDeViajes = length . viajes

    -- c

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje unViaje unChofer
    | aceptaViaje unChofer unViaje = unChofer{viajes = unViaje : viajes unChofer}
    | otherwise = unChofer

-- 7

    -- a

nito :: Chofer
nito = Chofer "Nito" 70000 (repetirViaje (Viaje 11032027 ("Lucas","Campus") 50)) (condicionCantidadLetras 3)

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

    --b.
{- 
No se puede calcular la liquidación del chofer ya que nunca terminaríamos
de mapear la infinita lista de viajes que tiene con Lucas, por ende nunca se llega
a sumar el monto y si así fuera, este no terminaría nunca de realizar la suma ya que
nunca termina la lista.
-}  

    --c.
{-
Sí, podemos saber esto ya que no involucra los viajes realizados, por ende se ejecuta sin
problemas.
-}

-- 8
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3