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