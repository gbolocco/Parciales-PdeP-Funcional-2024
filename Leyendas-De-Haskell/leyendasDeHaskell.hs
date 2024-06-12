data Investigador = Investigador{
    nombre :: String,
    experiencia :: Float,
    pokemonPrincipal :: Pokemon,
    mochila :: [Item],
    pokemonesCapturados :: [Pokemon]
}

data Pokemon = Pokemon{
    mote :: String,
    descripcion :: String,
    nivel :: Int,
    puntos :: Float
}deriving(Show)

pikachu :: Pokemon
pikachu = Pokemon "pikachu" "tipo rayo" 5 19


-- 1
akari :: Investigador
akari = Investigador "akari" 1499 oshawott [] [pikachu]

oshawott :: Pokemon
oshawott = Pokemon "oshawott" "una nutria que pelea con el caparazón de su pecho" 5 3

-- 2

saberRango :: Investigador -> String
saberRango unInvestigador
    | (<100) . experiencia $ unInvestigador = "Cielo"
    | (<500) . experiencia $ unInvestigador = "Estrella"
    | (<2000) . experiencia $ unInvestigador = "Constelacion"
    | otherwise = "Galaxia"

esAlfa :: Pokemon -> Bool
esAlfa unPokemon = comienzaConAlfa (mote unPokemon) || True

comienzaConAlfa :: String -> Bool
comienzaConAlfa mote = take 4 mote == "alfa" || take 4 mote == "ALFA"

-- 3

type Actividad = Investigador -> Investigador
type Item = Investigador -> Investigador

-- ==========

obtenerUnItem :: Item -> Actividad
obtenerUnItem unItem unInvestigador = unInvestigador{mochila = unItem : mochila unInvestigador}

    -- Items:
bayas :: Item
bayas = modificarExperiencia ((**2) . (+ 1))

apricorns :: Item
apricorns = modificarExperiencia (*1.5)

guijarros :: Item
guijarros = modificarExperiencia (+2)

fragmentos :: Float -> Item
fragmentos cantidad = modificarExperiencia (/ cantidad)

-- ==========

admirarPaisaje :: Actividad
admirarPaisaje unInvestigador = modificarExperiencia (*0.95) unInvestigador{mochila = drop 3 (mochila unInvestigador)} 

-- ==========

capturarUnPokemon :: Pokemon -> Actividad -- PROBAR ESTA FUNCION
capturarUnPokemon unPokemon unInvestigador
    | (>20).puntos $ unPokemon = agregarPokemon (pokemonPrincipal unInvestigador) . modificarExperiencia (+ puntos unPokemon) $ unInvestigador{pokemonPrincipal = unPokemon } 
    | otherwise = agregarPokemon unPokemon unInvestigador

-- ==========

combatirUnPokemon :: Pokemon -> Actividad
combatirUnPokemon unPokemon unInvestigador
    | ganaCombate (pokemonPrincipal unInvestigador) unPokemon = modificarExperiencia (+ puntos unPokemon / 2) unInvestigador
    | otherwise = unInvestigador

ganaCombate :: Pokemon -> Pokemon -> Bool
ganaCombate pokemon1 pokemon2 = nivel pokemon1 > nivel pokemon2 

-- ==========

-- Extraccion de logica
modificarExperiencia :: (Float -> Float) -> Investigador -> Investigador
modificarExperiencia funcion unInvestigador = unInvestigador{experiencia = funcion (experiencia unInvestigador)}

agregarPokemon :: Pokemon -> Investigador -> Investigador
agregarPokemon unPokemon unInvestigador = unInvestigador{pokemonesCapturados = unPokemon : pokemonesCapturados unInvestigador}

-- 4

type Expedicion = [Actividad]

actividadesRealizadasYFiltradas :: (Investigador -> a) -> (Investigador -> Bool) -> [Investigador] -> Expedicion -> [a]
actividadesRealizadasYFiltradas transformacion condicion investigadores = map transformacion . filter condicion . explorar investigadores

    -- a

reportePokemonesAlfa :: [Investigador] -> Expedicion -> [String]
reportePokemonesAlfa = actividadesRealizadasYFiltradas nombre tieneTresAlfas

tieneTresAlfas :: Investigador -> Bool
tieneTresAlfas unInvestigador = (>3).length.filter esAlfa $ (pokemonPrincipal unInvestigador : pokemonesCapturados unInvestigador)

    -- b

reporteRango :: [Investigador] -> Expedicion -> [Float]
reporteRango = actividadesRealizadasYFiltradas experiencia esRangoGalaxia

esRangoGalaxia :: Investigador -> Bool
esRangoGalaxia = (== "Galaxia") . saberRango

    -- c

alMenosUnPokemonPrincipalNivelDiez :: [Investigador] -> Expedicion -> [Pokemon]
alMenosUnPokemonPrincipalNivelDiez = actividadesRealizadasYFiltradas pokemonPrincipal tienePokemonNivelDiez

tienePokemonNivelDiez :: Investigador -> Bool
tienePokemonNivelDiez = (>10) . nivel . pokemonPrincipal

    -- d

ultimosTreaPokemonesNivelPar :: [Investigador] -> Expedicion -> [[Pokemon]]
ultimosTreaPokemonesNivelPar = actividadesRealizadasYFiltradas pokemonesCapturados tieneUltimosTresPokemonesNivelPar

tieneUltimosTresPokemonesNivelPar :: Investigador -> Bool
tieneUltimosTresPokemonesNivelPar = all ((==0) . flip mod 2 . nivel) . take 3 . reverse . pokemonesCapturados

-- Extraccion de logica

explorar :: [Investigador] -> Expedicion -> [Investigador]
explorar investigadores expedicion =  map (\x -> foldr ($) x expedicion) investigadores -- !!!!!!!!!!!

-- extra
explorarHierba :: Expedicion
explorarHierba = [capturarUnPokemon pikachu, admirarPaisaje, obtenerUnItem guijarros, obtenerUnItem bayas]
