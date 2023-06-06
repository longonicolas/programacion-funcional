module Parcial where
import Text.Show.Functions()

sumar :: Num a => a -> a -> a
sumar = (+)

type Nombre = String
type Victoria = (Oponente, Fecha)
type Oponente = String
type Fecha = Int

data Personaje = Personaje{

    nombre :: Nombre,
    poder :: Int,
    listaDeVictorias :: [Victoria]
}deriving Show

-- personajes prueba

personajeA = Personaje "Juan" 10 []
personajeB = Personaje "Jun" 15 []
personajeC = Personaje "Ju" 22 []

-- PARTE A

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento grupoDePersonajes = map (multiplicarPoder (length grupoDePersonajes)) grupoDePersonajes
multiplicarPoder :: Int -> Personaje -> Personaje
multiplicarPoder multiplicador unPersonaje = unPersonaje{poder = poder unPersonaje * multiplicador}

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos grupoDePersonajes = filter esRivalDigno . entrenamiento $ grupoDePersonajes
esRivalDigno :: Personaje -> Bool
esRivalDigno unPersonaje = poder unPersonaje > 500 && encontrarVictoria "Hijo de Thanos" unPersonaje
encontrarVictoria :: String -> Personaje -> Bool
encontrarVictoria nombrePerdedor unPersonaje = any (\ (a,_) -> a == nombrePerdedor) $ listaDeVictorias unPersonaje
encontrarVictoria' :: String -> Personaje -> Bool 
encontrarVictoria' nombrePerdedor unPersonaje = elem nombrePerdedor (map fst (listaDeVictorias unPersonaje)) --elegir esta
encontrarVictoria'' :: String -> Personaje -> Bool 
encontrarVictoria'' nombrePerdedor unPersonaje = elem nombrePerdedor (map fst . listaDeVictorias $ unPersonaje)

guerraCivil :: Fecha -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil anio grupo1 grupo2 = zipWith (pelear anio) grupo1 grupo2
pelear :: Fecha -> Personaje -> Personaje -> Personaje
pelear anio unPersonaje otroPersonaje
        | poder unPersonaje > poder otroPersonaje =  agregarVictoria unPersonaje (nombre otroPersonaje) anio
        | otherwise = agregarVictoria otroPersonaje (nombre unPersonaje) anio
agregarVictoria :: Personaje -> String -> Fecha -> Personaje
agregarVictoria unPersonaje nombrePerdedor anio = unPersonaje{listaDeVictorias = (nombrePerdedor,anio) : listaDeVictorias unPersonaje}

--substract

