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
    listaDeVictorias :: [Victoria],
    objetosEquipados :: [Equipamiento]
}deriving Show

-- personajes prueba

personajeA = Personaje "Thor" 10 [("Thanos",2010),("Thanos",2010),("Thanos",2010),("Thanos",2010),("Thanos",2010),("Thanos",2010)] []
personajeB = Personaje "Thanos" 10 [] []
personajeC = Personaje "Ju" 22 [] []

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
        | poder unPersonaje > poder otroPersonaje =  agregarVictoria (nombre otroPersonaje,anio) unPersonaje
        | otherwise = agregarVictoria (nombre unPersonaje,anio) otroPersonaje
agregarVictoria ::  Victoria -> Personaje -> Personaje
agregarVictoria victoria unPersonaje = unPersonaje{listaDeVictorias = victoria : listaDeVictorias unPersonaje}

--substract

-- PARTE B

type Equipamiento = Personaje -> Personaje

escudo :: Equipamiento
escudo unPersonaje 
        | (<5) . length . listaDeVictorias $ unPersonaje = modificarPoder 50 unPersonaje
        | otherwise = modificarPoder (-100) unPersonaje

modificarPoder :: Int -> Personaje -> Personaje
modificarPoder cantidad unPersonaje = unPersonaje{ poder = poder unPersonaje + cantidad}

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado version unPersonaje = editarNombreIron . agregarVersionTraje version $ unPersonaje
agregarVersionTraje :: Int -> Personaje -> Personaje
agregarVersionTraje version unPersonaje = unPersonaje{nombre = nombre unPersonaje ++ " V" ++ show version}
editarNombreIron :: Personaje -> Personaje 
editarNombreIron unPersonaje = editarNombre "Iron " unPersonaje

editarNombre :: String -> Personaje -> Personaje
editarNombre caracteres unPersonaje = unPersonaje{nombre = caracteres ++ nombre unPersonaje}

verificarNombre :: String -> String -> Bool
verificarNombre unNombre nombreRequerido = unNombre == nombreRequerido

equipamientoExclusivo :: String -> (Personaje -> Personaje) -> Personaje -> Personaje
equipamientoExclusivo nombreExclusivo accion unPersonaje
        | verificarNombre (nombre unPersonaje) nombreExclusivo = accion unPersonaje
        | otherwise = unPersonaje

stormBreaker :: Equipamiento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" (editarNombre "dios del trueno " . limpiarVictorias) $ unPersonaje
limpiarVictorias :: Personaje -> Personaje
limpiarVictorias unPersonaje = unPersonaje{listaDeVictorias = []}

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje =  equipamientoExclusivo "Thanos" (agregarVictoriaInfinita listaInfinita) unPersonaje 

agregarVictoriaInfinita :: [Victoria] -> Personaje -> Personaje
agregarVictoriaInfinita listaDeVencidos unPersonaje = unPersonaje{listaDeVictorias = listaDeVencidos ++ listaDeVictorias unPersonaje}

listaInfinita :: [Victoria]
listaInfinita = zip extrasInfinitos aniosInfinitos
extrasInfinitos :: [String]
extrasInfinitos = map (\unNumero -> ("extra numero " ++ show unNumero)) [1 ..]
aniosInfinitos :: [Int]
aniosInfinitos = [2023 ..]

guanteleteInfinito :: [Equipamiento] -> Equipamiento
guanteleteInfinito listaDeEquipamientos unPersonaje = foldl agregarEquipamiento unPersonaje (filter esGemaDelInfinito listaDeEquipamientos)

agregarEquipamiento :: Personaje -> Equipamiento -> Personaje
agregarEquipamiento unPersonaje equipamiento = unPersonaje{objetosEquipados = equipamiento : objetosEquipados unPersonaje}

-- PARTE C
{- 

a) No va a funcionar debido a que no termina. Ya que la funcion utiliza "length" para obtener el total de elementos de la lista. Que es infinita
b) Al utilizarlo en black widow. El programa se congela ya que se busca un parametro infinitamente en el caso de que tenga poder mayor a 500. (Y no tenga hijo de thanos)
c) Yes

-}
 