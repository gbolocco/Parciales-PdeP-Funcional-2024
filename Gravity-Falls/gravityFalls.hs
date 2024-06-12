-- 1

data Persona = Persona {
    edad :: Int,
    items :: [Item],
    experiencia :: Float
}deriving(Show)

 -- (Nombre,Peligrosidad)

data Criatura = Criatura{
    peligrosidad :: Float,
    accionPorCumplir :: (Persona -> Bool)
}

siempreDetras :: Criatura
siempreDetras = Criatura 0 (const True)

gnomo :: Float -> Criatura
gnomo cantidad = Criatura (2 ** cantidad) (tenerItem "soplete")

tenerItem :: String -> Persona -> Bool
tenerItem unItem unaPersona = elem unItem (items unaPersona)

fantasma :: Float -> (Persona -> Bool) -> Criatura --categoria del 1 al 10 ASUNTO PENDIENTE??
fantasma categoria conflicto = Criatura (categoria * 20) conflicto

type Item = String 

-- 2
enfrentarCriatura :: Persona -> Criatura -> Persona
enfrentarCriatura unaPersona unaCriatura
    | gana unaPersona unaCriatura = modificarExperiencia (+ peligrosidad unaCriatura) unaPersona
    | otherwise = modificarExperiencia (+1) unaPersona

gana :: Persona -> Criatura -> Bool
gana unaPersona unaCriatura = accionPorCumplir unaCriatura unaPersona

modificarExperiencia :: (Float -> Float) -> Persona -> Persona
modificarExperiencia funcion unaPersona = unaPersona{experiencia = funcion (experiencia unaPersona)}

-- 3
    -- a

experienciaCapazDeGanar :: Persona -> [Criatura] -> Float
experienciaCapazDeGanar unaPersona criaturas = experiencia (foldl enfrentarCriatura unaPersona criaturas) - experiencia unaPersona

{-
unasCriaturas :: [Criatura]
unasCriaturas = [gnomo 10, fantasma 3 asunto1, fantasma 1 ((>10).experiencia)]

tioStan :: Persona
tioStan = Persona 60 ["soplete"] 200

asunto1 :: (Persona -> Bool)
asunto1 unaPersona = ((<13).edad $ unaPersona) && tenerItem "disfraz de oveja" unaPersona

experienciaCapazDeGanar tioStan unasCriaturas 
1245.0

-}

-- SEGUNDA PARTE

--zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]

--zipWithIf  _ _ _ [] = []
--zipWithIf _ _ [] _  = [] 
--zipWithIf funcion condicion (x:xs) (y:ys)
--    | funcion 