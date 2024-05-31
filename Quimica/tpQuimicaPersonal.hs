data Sustancia = 
    
    Elemento {
        nombreElemento :: String,
        simboloQuimico :: String,
        numeroAtomico :: Int,
        grupo :: Grupo
    }
    |
    Compuesto {
        nombreCompuesto :: String,
        componentes :: [Componente],
        grupo :: Grupo

    } deriving(Show)

type Componente = (Sustancia,Int)
data Grupo = Metal | NoMetal | GasNoble | Halogeno deriving(Show)

{-
1. Modelar las siguientes sustancias:
    a. El hidrógeno y el oxígeno
    b. El agua, sustancia compuesta por 2 hidrógenos y 1 un oxígeno.
-}

hidrogeno :: Sustancia
hidrogeno = Elemento{
    nombreElemento = "hidrogeno",
    simboloQuimico = "H",
    numeroAtomico = 1,
    grupo = NoMetal
}
oxigeno :: Sustancia
oxigeno = Elemento{
    nombreElemento = "oxigeno",
    simboloQuimico = "O",
    numeroAtomico = 8,
    grupo = NoMetal
}

oro :: Sustancia
oro = Elemento {
    nombreElemento = "Oro",
    simboloQuimico = "Au",
    numeroAtomico = 72,
    grupo = Metal
}

nitrogeno :: Sustancia
nitrogeno = Elemento{
    nombreElemento = "nitrogeno",
    simboloQuimico = "N",
    numeroAtomico = 12,
    grupo = NoMetal
}

agua :: Sustancia
agua = Compuesto{
    nombreCompuesto = "agua",
    componentes = [(hidrogeno,2),(oxigeno,1)],
    grupo = NoMetal
}

dioxidoDeNitrogeno :: Sustancia
dioxidoDeNitrogeno = Compuesto{
    nombreCompuesto = "dioxidoDeNitrogeno",
    componentes = [(nitrogeno, 1),(oxigeno, 2)],
    grupo = NoMetal
}

gianlucogeno :: Sustancia
gianlucogeno = Compuesto{
    nombreCompuesto = "dioxidoDeNitrogeno",
    componentes = [(agua,2),(oro,3),(dioxidoDeNitrogeno,4)],
    grupo = NoMetal
}

--  2 Conduce Bien

data Criterio = Electricidad | Calor deriving(Show,Eq)

conduceBien :: Sustancia -> Criterio -> Bool

conduceBien (Elemento _ _ _ Metal ) _ = True
conduceBien (Compuesto _ _ Metal ) _ = True
conduceBien (Elemento _ _ _ GasNoble ) Electricidad = True -- PATTERN MATCHING
conduceBien (Compuesto _ _ Halogeno ) Calor = True

conduceBien _ _ = False --El resto de todos los casos

-- 3 Nombre de Union

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

nombreDeUnion :: String -> String
nombreDeUnion nombre
    | (not . esVocal . last) nombre = nombre ++ "uro"
    | otherwise = (nombreDeUnion . init) nombre
    
-- 4 Combinar Dos Nombres

combinarDosNombres :: String -> String -> String
combinarDosNombres unaSustancia otraSustancia = nombreDeUnion unaSustancia ++ " de " ++ otraSustancia

-- 5 mezclar

mezclar :: Componente -> Componente -> Sustancia
mezclar primerComponente segundoComponente =  Compuesto {
    nombreCompuesto = combinarDosNombres (nombreElemento . fst $ primerComponente) (nombreElemento . fst $ segundoComponente),
    componentes = [primerComponente, segundoComponente],
    grupo = NoMetal
}

{-
6. Obtener la fórmula de una sustancia:
    ● para los elementos es su símbolo químico
    ● para los compuestos es la concatenación de las representaciones de sus componentes y se pone entre paréntesis

La representación de un componente depende de la cantidad de moléculas:
    ● si tiene una, entonces solo es la fórmula de su sustancia
    ● si tiene más, entonces es la fórmula de su sustancia y la cantidad
Por ejemplo, la fórmula del agua debería ser (H2O).


obtenerFormula :: Sustancia -> String

obtenerFormula (Elemento _ simboloQuimico _ _) = simboloQuimico
obtenerFormula (Compuesto _ _ componentes) = concat (map representacion componentes)


representacion (componentes sustancia 1) = obtenerFormula sustancia
representacion (componentes sustancia numero) = obtenerFormula sustancia ++ show numero
-}

obtenerFormula :: Sustancia -> String
obtenerFormula (Elemento _ simbolo _ _) = simbolo
obtenerFormula (Compuesto _ componentes _) = "(" ++ concat (map representacion componentes) ++ ")"

representacion :: Componente -> String
representacion (sustancia, 1) = obtenerFormula sustancia
representacion (sustancia, numero) = obtenerFormula sustancia ++ show numero

{-
ghci> obtenerFormula gianlucogeno
"((H2O)2Au3(NO2)4)"
-}