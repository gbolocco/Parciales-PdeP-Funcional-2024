data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: (Float,Float), -- (rueda,chasis)
    velMax :: Float,
    tiempoDeCarrera :: Float
}deriving(Show)

-- 1

ferrariF50 :: Auto
ferrariF50 = Auto {
    marca = "ferrari",
    modelo = "f50",
    desgaste = (0,0),
    velMax = 65,
    tiempoDeCarrera = 0
}

lamborghiniDiablo :: Auto
lamborghiniDiablo = Auto {
    marca = "lamborghini",
    modelo = "diablo",
    desgaste = (4,7),
    velMax = 73,
    tiempoDeCarrera = 0
}

fiat600 :: Auto
fiat600 = Auto {
    marca = "fiat",
    modelo = "600",
    desgaste = (79,79),
    velMax = 18,
    tiempoDeCarrera = 0
}

laChataDeFede :: Auto
laChataDeFede = Auto {
    marca = "ford",
    modelo = "ranger",
    desgaste = (70,82),
    velMax = 14,
    tiempoDeCarrera = 0
}

toyotaCorolla :: Auto
toyotaCorolla = Auto {
    marca = "Toyota",
    modelo = "Corolla",
    desgaste = (10, 20),
    velMax = 60,
    tiempoDeCarrera = 0
}

chevroletCamaro :: Auto
chevroletCamaro = Auto {
    marca = "Chevrolet",
    modelo = "Camaro",
    desgaste = (5, 15),
    velMax = 70,
    tiempoDeCarrera = 0
}

hondaCivic :: Auto
hondaCivic = Auto {
    marca = "Honda",
    modelo = "Civic",
    desgaste = (15, 25),
    velMax = 55,
    tiempoDeCarrera = 0
}

-- 2 Estado del auto

buenEstado :: Auto -> Bool
buenEstado unAuto = ((<60).fst.desgaste $ unAuto) && ((<40).snd.desgaste $ unAuto) --fst y snd (ruedas,chasis)

noDaMas :: Auto -> Bool
-- noDaMas unAuto = any (>80) (desgaste unAuto) por alguna razon any no funciona con tuplas¿
noDaMas unAuto = ((>80) . fst . desgaste $ unAuto) || ((>80) . snd . desgaste $ unAuto)

-- 3 Reparar
reparar :: Auto -> Auto
reparar unAuto = unAuto {desgaste = (0,repararChasis unAuto)}

repararChasis :: Auto -> Float
repararChasis unAuto = (snd.desgaste $ unAuto) - (snd.desgaste $ unAuto) * 0.85

-- 4

--Funciones de desgaste
desgastarRuedas :: Auto -> Float -> Float
desgastarRuedas unAuto valor = (fst.desgaste $ unAuto) + valor 
desgastarChasis :: Auto -> Float -> Float
desgastarChasis unAuto valor = (snd.desgaste $ unAuto) + valor
--Funcion para sumar tiempo de carrera
nuevoTiempoDeCarrera :: Auto -> Float -> Float
nuevoTiempoDeCarrera unAuto unTiempo = tiempoDeCarrera unAuto + unTiempo

type Tramo = Auto -> Auto

-- a

curva :: Float -> Float -> Tramo
curva longitud angulo unAuto = unAuto {desgaste = (desgastarRuedas unAuto ((3 * longitud) / angulo) , desgastarChasis unAuto 0), tiempoDeCarrera = nuevoTiempoDeCarrera unAuto (longitud / ( velMax unAuto /2))}

-- i
curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300 --no hace falta pasarle el parametro del auto porque ya lo infiere el mismisimo haskell

-- ii
curvaTranca :: Tramo
curvaTranca = curva 110 550

-- b

recta :: Float -> Tramo
recta longitud unAuto = unAuto {desgaste = (desgastarRuedas unAuto 0, desgastarChasis unAuto (longitud / 100)), tiempoDeCarrera = nuevoTiempoDeCarrera unAuto (longitud / velMax unAuto)}

-- i
tramito :: Tramo
tramito = recta 280
-- ii
tramoRectoClassic :: Tramo
tramoRectoClassic = recta 750

-- c

boxes :: Float -> Tramo
boxes longitud unAuto
    |noDaMas unAuto = (reparar unAuto){tiempoDeCarrera = nuevoTiempoDeCarrera unAuto (longitud / velMax unAuto + 10)}
    |otherwise = recta longitud unAuto

-- d
tramoMojado :: Tramo -> Tramo
tramoMojado unTramo unAuto = (unTramo unAuto){tiempoDeCarrera = nuevoTiempoDeCarrera unAuto (((tiempoDeCarrera.unTramo $ unAuto) - tiempoDeCarrera unAuto) / 2) } --(tiempoNuevo - tiempoViejo) / 2  
-- e
ripio :: Tramo -> Tramo
ripio unTramo = unTramo.unTramo
-- f 
tramoDesgastado :: Float -> Tramo -> Tramo --Float -> Auto -> Auto ->Auto -> Auto
tramoDesgastado longitud unTramo unAuto = (unTramo unAuto){desgaste = (desgastarRuedas (unTramo unAuto) (longitud*2),desgastarChasis (unTramo unAuto) 0)}

-- 5
{--}
pasarPorTramo :: Tramo -> Auto -> Auto
pasarPorTramo unTramo = unTramo

-- 6
-- a
type Pista = [Tramo]
superPista :: [Tramo]
superPista = [tramoRectoClassic,curvaTranca,tramoMojado tramito,tramito,tramoDesgastado 2 (curva 400 80),curva 650 115, recta 970, ripio tramito, boxes 800]
-- b
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta _ [] = [] -- si no hay autos
peganLaVuelta [] autos = autos -- si no hay tramos en la pista
peganLaVuelta (tramo:restoPista) autos = peganLaVuelta restoPista (map (pasarPorTramo tramo) autos)
--peganLaVuelta (tramo:restoPista) autos = peganLaVuelta restoPista (filter (not . noDaMas) (map (pasarPorTramo tramo) autos))

-- 7

-- a

carrera :: [Auto] -> Pista -> Int -> [Auto]
carrera autos pista numVueltas = iterate (peganLaVuelta pista) autos !! numVueltas
--carrera autos pista numVueltas = filter (not . noDaMas) (iterate (peganLaVuelta pista) autos !! numVueltas) -- aca hago el filtrado de los autos que no dan mas ¿

-- b

listaAutos :: [Auto]
listaAutos = [ferrariF50, lamborghiniDiablo, fiat600, laChataDeFede, toyotaCorolla, chevroletCamaro, hondaCivic]

tourBuenosAires :: [Auto] -> [Auto]
tourBuenosAires autos = carrera autos superPista 20

-- c

type ResultadosParciales = [[Auto]]

jugarCarrera :: [Auto] -> Pista -> Int -> ResultadosParciales
jugarCarrera autos pista numVueltas = take numVueltas (iterate (realizarVuelta pista) autos)

realizarVuelta :: Pista -> [Auto] -> [Auto]
realizarVuelta pista autos = peganLaVuelta pista (filter (not . noDaMas) autos)

{-
jugarCarrera listaAutos superPista 10

[[Auto {marca = "ferrari", modelo = "f50", desgaste = (0.0,0.0), velMax = 65.0, tiempoDeCarrera = 0.0},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (4.0,7.0), velMax = 73.0, tiempoDeCarrera = 0.0},
Auto {marca = "fiat", modelo = "600", desgaste = (79.0,79.0), velMax = 18.0, tiempoDeCarrera = 0.0},
Auto {marca = "ford", modelo = "ranger", desgaste = (70.0,82.0), velMax = 14.0, tiempoDeCarrera = 0.0},
Auto {marca = "Toyota", modelo = "Corolla", desgaste = (10.0,20.0), velMax = 60.0, tiempoDeCarrera = 0.0},
Auto {marca = "Chevrolet", modelo = "Camaro", desgaste = (5.0,15.0), velMax = 70.0, tiempoDeCarrera = 0.0},
Auto {marca = "Honda", modelo = "Civic", desgaste = (15.0,25.0), velMax = 55.0, tiempoDeCarrera = 0.0}],

[Auto {marca = "ferrari", modelo = "f50", desgaste = (36.556522,36.399998), velMax = 65.0, tiempoDeCarrera = 89.53847},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (40.556522,43.399998), velMax = 73.0, tiempoDeCarrera = 79.72603},
Auto {marca = "fiat", modelo = "600", desgaste = (0.0,16.11), velMax = 18.0, tiempoDeCarrera = 333.3333},
Auto {marca = "Toyota", modelo = "Corolla", desgaste = (46.556522,56.399998), velMax = 60.0, tiempoDeCarrera = 96.99999},
Auto {marca = "Chevrolet", modelo = "Camaro", desgaste = (41.556522,51.399998), velMax = 70.0, tiempoDeCarrera = 83.14286},
Auto {marca = "Honda", modelo = "Civic", desgaste = (51.55652,61.399998), velMax = 55.0, tiempoDeCarrera = 105.81819}],

[Auto {marca = "ferrari", modelo = "f50", desgaste = (73.113045,72.799995), velMax = 65.0, tiempoDeCarrera = 179.07693},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (77.113045,79.8), velMax = 73.0, tiempoDeCarrera = 159.45206},
Auto {marca = "fiat", modelo = "600", desgaste = (36.556522,52.51), velMax = 18.0, tiempoDeCarrera = 656.6666},
Auto {marca = "Toyota", modelo = "Corolla", desgaste = (0.0,12.720001), velMax = 60.0, tiempoDeCarrera = 204.0},
Auto {marca = "Chevrolet", modelo = "Camaro", desgaste = (78.113045,87.8), velMax = 70.0, tiempoDeCarrera = 166.28574},
Auto {marca = "Honda", modelo = "Civic", desgaste = (0.0,13.470001), velMax = 55.0, tiempoDeCarrera = 221.6364}],

[Auto {marca = "ferrari", modelo = "f50", desgaste = (0.0,15.18), velMax = 65.0, tiempoDeCarrera = 278.61542},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (0.0,16.229996), velMax = 73.0, tiempoDeCarrera = 249.1781},
Auto {marca = "fiat", modelo = "600", desgaste = (0.0,12.1364975), velMax = 18.0, tiempoDeCarrera = 990.0},
Auto {marca = "Toyota", modelo = "Corolla", desgaste = (36.556522,49.12), velMax = 60.0, tiempoDeCarrera = 300.99997},
Auto {marca = "Honda", modelo = "Civic", desgaste = (36.556522,49.87), velMax = 55.0, tiempoDeCarrera = 327.45456}],

[Auto {marca = "ferrari", modelo = "f50", desgaste = (36.556522,51.579998), velMax = 65.0, tiempoDeCarrera = 368.15378},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (36.556522,52.629993), velMax = 73.0, tiempoDeCarrera = 328.90408},
Auto {marca = "fiat", modelo = "600", desgaste = (36.556522,48.536495), velMax = 18.0, tiempoDeCarrera = 1313.3331},
Auto {marca = "Toyota", modelo = "Corolla", desgaste = (73.113045,85.520004), velMax = 60.0, tiempoDeCarrera = 397.9999},
Auto {marca = "Honda", modelo = "Civic", desgaste = (73.113045,86.270004), velMax = 55.0, tiempoDeCarrera = 433.27267}],

[Auto {marca = "ferrari", modelo = "f50", desgaste = (73.113045,87.98), velMax = 65.0, tiempoDeCarrera = 457.69214},
Auto {marca = "lamborghini", modelo = "diablo", desgaste = (0.0,12.154495), velMax = 73.0, tiempoDeCarrera = 418.63004},
Auto {marca = "fiat", modelo = "600", desgaste = (73.113045,84.9365), velMax = 18.0, tiempoDeCarrera = 1636.6664}],

[Auto {marca = "lamborghini", modelo = "diablo", desgaste = (36.556522,48.554493), velMax = 73.0, tiempoDeCarrera = 498.356}],
[Auto {marca = "lamborghini", modelo = "diablo", desgaste = (73.113045,84.9545), velMax = 73.0, tiempoDeCarrera = 578.08215}],

[],

[]]
-}
