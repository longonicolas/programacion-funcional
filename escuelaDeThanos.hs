import Text.Show.Functions

data Personaje = Personaje{

    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
} deriving (Show, Eq)

data Guantelete = Guantelete{

    material :: String,
    gemas :: [Gema]
}deriving Show

type Gema = Personaje -> Personaje
type Universo = [Personaje]

player1 = Personaje 40 100 ["correr"] "Juan" "Melmac"

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto unGuantelete = cantGemas unGuantelete == 6 && (material unGuantelete) == "uru"

cantGemas :: Guantelete -> Int
cantGemas = length . gemas

chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso unGuantelete universo 
        | guanteleteCompleto unGuantelete = reducirUniverso universo
        | otherwise = universo

reducirUniverso :: Universo -> Universo
reducirUniverso universo = take (mitadUniverso universo) universo

mitadUniverso :: Universo -> Int
mitadUniverso universo = div (length universo) 2

aptoPendex :: Universo -> Bool
aptoPendex universo = any(<45) . map edad $ universo

tieneMasDeNHabilidades :: Int -> Personaje -> Bool
tieneMasDeNHabilidades cant personaje = (>cant) . length . habilidades $ personaje

filtrarPersonajesHabilidosos :: [Personaje] -> [Personaje]
filtrarPersonajesHabilidosos personajes = filter (tieneMasDeNHabilidades 1) $ personajes

energiaTotal :: Universo -> Int
energiaTotal universo = sum . map energia . filtrarPersonajesHabilidosos $ universo

-- PARTE B

modificarEnergia :: (Int -> Int) -> Personaje -> Personaje
modificarEnergia funcion pj = pj{energia = funcion . energia $ pj}

modificarHabilidad :: ([String] -> [String]) -> Personaje -> Personaje
modificarHabilidad funcion pj = pj{habilidades = funcion . habilidades $ pj}

modificarEdad :: (Int -> Int) -> Personaje -> Personaje
modificarEdad funcion pj = pj{edad = max 18 . funcion . edad $ pj}

gemaMente :: Int -> Gema
gemaMente cantidad = modificarEnergia (subtract cantidad)

gemaAlma :: String -> Gema
gemaAlma unaHabilidad unPersonaje = modificarEnergia (subtract 10) (modificarHabilidad (filter(not.tieneLaHabilidad unaHabilidad)) unPersonaje)

tieneLaHabilidad :: String -> String -> Bool
tieneLaHabilidad habilidad1 habilidad2 = habilidad1 == habilidad2

gemaEspacio :: String -> Gema
gemaEspacio unPlaneta unPersonaje = modificarEnergia (subtract 20) (cambiarPlaneta unPlaneta unPersonaje)

cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta unPlaneta unPersonaje = unPersonaje{planeta = unPlaneta}

gemaPoder :: Gema
gemaPoder unPersonaje = modificarEnergia (subtract . energia $ unPersonaje) (analizarHabilidades unPersonaje)

analizarHabilidades :: Personaje -> Personaje
analizarHabilidades unPersonaje
        | tieneMasDeNHabilidades 2 unPersonaje = unPersonaje
        | otherwise = modificarHabilidad (const []) unPersonaje


gemaTiempo :: Gema
gemaTiempo unPersonaje = modificarEnergia (subtract 50) . modificarEdad (const mitadEdad (edad unPersonaje)) $ unPersonaje

mitadEdad :: Int -> Int
mitadEdad edad = div edad 2

gemaLoca :: Gema -> Personaje -> Personaje
gemaLoca gema unPersonaje = gema . gema $ unPersonaje

guanteleteGoma :: Guantelete
guanteleteGoma = Guantelete "goma" [gemaTiempo,gemaAlma "Nvoljnik", gemaLoca (gemaAlma "haskell")]

utilizar :: [Gema] -> Personaje -> Personaje
utilizar gemas pj = foldr ($) pj gemas

