import Text.Show.Functions ()

data Ninja = Ninja {
  nombre       :: String,
  herramientas :: [Herramienta],
  jutsus       :: [Jutsu],
  rango        :: Int
} deriving Show

type Herramienta = (String, Int)

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango unaFuncion uneNinja = uneNinja { rango = max 0 . unaFuncion . rango $ uneNinja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta
mapCantidad unaFuncion (unNombre, unaCantidad) = (unNombre, unaFuncion unaCantidad)

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas unaFuncion uneNinja = uneNinja { herramientas = unaFuncion . herramientas $ uneNinja }

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

-- A.

-- A.a.

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta unaHerramienta uneNinja = agregarHerramienta (limitarCantidadA (cuantasHerramientasPuedeObtener uneNinja) unaHerramienta) uneNinja

limitarCantidadA :: Int -> Herramienta -> Herramienta
limitarCantidadA unaCantidad = mapCantidad (min unaCantidad)

agregarHerramienta :: Herramienta -> Ninja -> Ninja
agregarHerramienta unaHerramienta = mapHerramientas (unaHerramienta :)

cuantasHerramientasPuedeObtener :: Ninja -> Int
cuantasHerramientasPuedeObtener = (100 -) . cantidadDeHerramientas

cantidadDeHerramientas :: Ninja -> Int
cantidadDeHerramientas = sum . map snd . herramientas

-- A.b.

usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta unNombreDeHerramienta uneNinja = mapHerramientas (filter ((/= unNombreDeHerramienta) . nombreHerramienta)) uneNinja

-- B

data Mision = Mision {
  cantidadDeNinjas :: Int,
  rangoRecomendado :: Int,
  ninjasEnemigos   :: [Ninja],
  recompensa       :: Herramienta
} deriving Show

type Equipo = [Ninja]

-- B.a.

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante unEquipo unaMision = tieneMiembroNoCalificadoPara unEquipo unaMision && ((>= 2) . length . ninjasEnemigos) unaMision

tieneMiembroNoCalificadoPara :: Equipo -> Mision -> Bool
tieneMiembroNoCalificadoPara unEquipo unaMision = any (not . estaCalificadoPara unaMision) unEquipo

estaCalificadoPara :: Mision -> Ninja -> Bool
estaCalificadoPara unaMision uneNinja = rango uneNinja >= rangoRecomendado unaMision

-- B.b.

esCopada :: Mision -> Bool
esCopada = esRecompensaCopada . recompensa

esRecompensaCopada :: Herramienta -> Bool
esRecompensaCopada unaHerramienta = elem unaHerramienta recompensasCopadas

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("Bomba de Humo", 3), ("Shuriken", 5), ("Kunai", 14)]

-- B.1.c.

esFactible :: Equipo -> Mision -> Bool
esFactible unEquipo unaMision = (not . esDesafiante unEquipo) unaMision && estaBienPreparadoPara unEquipo unaMision

estaBienPreparadoPara :: Equipo -> Mision -> Bool
estaBienPreparadoPara unEquipo unaMision = tieneSuficientesNinjasPara unEquipo unaMision || estanBienArmades unEquipo

tieneSuficientesNinjasPara :: Equipo -> Mision -> Bool
tieneSuficientesNinjasPara unEquipo unaMision = length unEquipo >= cantidadDeNinjas unaMision

estanBienArmades :: Equipo -> Bool
estanBienArmades = (> 500) . sum . map cantidadDeHerramientas

-- B.2.a.

fallarMision :: Mision -> Equipo -> Equipo
fallarMision = filter . estaCalificadoPara

-- B.2.b.

cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision unaMision = map (obtenerHerramienta (recompensa unaMision) . promover)

promover :: Ninja -> Ninja
promover = mapRango succ

-- B.3.a.

type Jutsu = Mision -> Mision

clonesDeSombra :: Int -> Jutsu
clonesDeSombra = mapCantidadDeNinjas . subtract

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas unaFuncion unaMision = unaMision { cantidadDeNinjas = max 1 . unaFuncion . cantidadDeNinjas $ unaMision }

-- B.3.b.

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = mapNinjasEnemigos (filter ((>= 500) . rango))

mapNinjasEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapNinjasEnemigos unaFuncion unaMision = unaMision { ninjasEnemigos = unaFuncion . ninjasEnemigos $ unaMision }

ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo = completarMision unEquipo . usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision
  | esCopada unaMision || esFactible unEquipo unaMision = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo

-- C.

granGuerraNinja :: Mision
granGuerraNinja = Mision {
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100,
  ninjasEnemigos   = infinitosZetsus,
  recompensa       = ("Abanico de Uchiha Madara", 1)
}

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu unNumero = Ninja {
  nombre       = "Zetsu " ++ show unNumero,
  rango        = 600,
  jutsus       = [],
  herramientas = []
}

-- C.a.

-- Si el equipo no tiene ningún miembro no calificado para la misión entonces termina de evaluar y devuelve False.
-- En caso contrario no termina de evaluar porque no se puede obtener la longitud de una lista infinita.
-- Se podría reescribir la segunda condición con drop de modo que pueda terminar de evaluar.

-- C.b.

-- Termina de evaluar y devuelve False ya que el abanico de uchiha madara no es una recompensa copada.

-- C.c.

-- Devuelve una misión con una lista infinita de ninjas enemigos.

{-
  En todos los casos la justificación es que dado que Haskell trabaja con evaluación perezosa no necesita evaluar
  la lista infinita de enemigos antes de empezar a evaluar la función. Esto permite que en los casos en los que no se
  necesita recorrer la lista entera de enemigos para llegar a un resultado se pueda dar una respuesta de todos modos.
-}
