--data Personaje = Personaje {
--    nombre:: String,
--    puntaje:: Int,
--    inventario:: [Material]
--} deriving(Show)

--type Material = (String,Int) -- (nombre,cantidad)

--type Receta = [String]
-- En ninguna receta hace falta más de una unidad del mismo material
--steve :: Personaje
--steve = Personaje "Steve" 100 [("tierra",3),("piedra",2),("madera",4)]

-- 1
--fogata :: Receta
--fogata = ["tierra","piedra","madera"]

--craftear :: Receta -> Personaje -> Personaje
--craftear unaReceta unPersonaje
--    | poseeMateriales unaReceta unPersonaje = unPersonaje
--    | otherwise = unPersonaje

--poseeMateriales :: Receta -> Personaje -> Bool
--poseeMateriales unaReceta unPersonaje = all (map (map poseeMaterial unaRececta) (map fst (inventario unPersonaje)))

--poseeMaterial :: String -> Personaje -> Bool
--poseeMaterial unMaterial unPersonaje = any ((== unMaterial) . fst) (inventario unPersonaje)

