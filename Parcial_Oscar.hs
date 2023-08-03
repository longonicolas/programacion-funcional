module Parcial where
import Text.Show.Functions()

data Ninja = Ninja{
    nombre::Nombre,
    herramientas::[Herramienta],
    jutsus::[Jutsu],
    rango::Int
} deriving Show


data Herramienta = Herramienta{
    nombreHerramienta::Nombre,
    cantidad::Int
} deriving (Show, Eq)

data Mision = Mision{
    cantidadRequerida::Int,
    rangoMision::Int,
    enemigos::[Ninja],
    recompensa::Herramienta
} deriving Show

type Nombre = String
type Jutsu = (Mision -> Mision)

cambiarHerramientas:: ([Herramienta]->[Herramienta]) -> Ninja -> Ninja
cambiarHerramientas modificador unNinja = unNinja{herramientas = modificador . herramientas $ unNinja}

agregarHerramienta:: Herramienta -> Ninja -> Ninja
agregarHerramienta nuevaHerramienta  = cambiarHerramientas (nuevaHerramienta:)


obtenerHerramienta:: Herramienta -> Ninja -> Ninja
obtenerHerramienta nuevaHerramienta unNinja
    | sumaDeSusHerramientas unNinja + cantidad nuevaHerramienta  <= 100 = agregarHerramienta nuevaHerramienta unNinja
    | otherwise = agregarHerramienta (descontarCantidadExedente nuevaHerramienta unNinja) unNinja

descontarCantidadExedente:: Herramienta -> Ninja -> Herramienta
descontarCantidadExedente unaHerramienta unNinja = unaHerramienta{cantidad = sumaDeSusHerramientas unNinja - 100}

sumaDeSusHerramientas:: Ninja -> Int
sumaDeSusHerramientas = sum . map cantidad . herramientas

usarHerramienta:: Herramienta -> Ninja -> Ninja
usarHerramienta unaHerramienta = cambiarHerramientas (filter (not.esLaMismaHerramienta unaHerramienta))

esLaMismaHerramienta:: Herramienta ->Herramienta -> Bool
esLaMismaHerramienta unaHerramienta otraHerramienta = nombreHerramienta unaHerramienta == nombreHerramienta otraHerramienta

--Parte B

esDesafiante:: Mision -> [Ninja] -> Bool
esDesafiante unaMision grupoDeNinjas = ((>=2) .length . enemigos) unaMision  && alguienTieneMenorRango unaMision grupoDeNinjas

alguienTieneMenorRango:: Mision -> [Ninja] -> Bool
alguienTieneMenorRango unaMision  = any (not . tieneRangoSuficiente unaMision)

tieneRangoSuficiente:: Mision -> Ninja -> Bool
tieneRangoSuficiente unaMision = (rangoMision unaMision <=). rango

esCopada:: Mision -> Bool
esCopada unaMision = recompensa unaMision `elem` recompensasCopadas

recompensasCopadas::[Herramienta]
recompensasCopadas = [Herramienta "Bomba de humo" 3 ,Herramienta "Shuriken" 5, Herramienta "Kunai" 14]

esFactible:: Mision -> [Ninja] -> Bool
esFactible unaMision grupoDeNinjas =  (not . esDesafiante unaMision) grupoDeNinjas && sumaTotalHerramientas grupoDeNinjas > 500 || hayCantidadNecesariaNinjas grupoDeNinjas unaMision

hayCantidadNecesariaNinjas:: [Ninja] -> Mision -> Bool
hayCantidadNecesariaNinjas grupoDeNinjas unaMision = cantidadRequerida unaMision  <= length grupoDeNinjas

sumaTotalHerramientas:: [Ninja] -> Int
sumaTotalHerramientas = sum . map sumaDeSusHerramientas

fallarMision:: Mision -> [Ninja] -> [Ninja]
fallarMision unaMision  = map (cambiarRango (subtract 2)) . filter (tieneRangoSuficiente unaMision)

cambiarRango:: (Int->Int) -> Ninja -> Ninja
cambiarRango modificador unNinja = unNinja{rango = max 0 . modificador . rango $ unNinja}

cumplirMision:: Mision -> [Ninja] -> [Ninja]
cumplirMision unaMision  = map ((agregarHerramienta . recompensa $ unaMision) . cambiarRango (+1))

clonesDeSombra:: Int -> Jutsu
clonesDeSombra cantidadClones = cambiarCantidadRequerida (subtract cantidadClones)

cambiarCantidadRequerida:: (Int->Int) -> Mision -> Mision
cambiarCantidadRequerida modificador unaMision = unaMision{cantidadRequerida = max 1 . modificador . cantidadRequerida $ unaMision}

cambiarEnemigos:: ([Ninja]->[Ninja]) -> Mision -> Mision
cambiarEnemigos modificador unaMision = unaMision{enemigos = modificador . enemigos $ unaMision}

fuerzaDeUnCentenar:: Jutsu
fuerzaDeUnCentenar = cambiarEnemigos (filter ((>=500). rango)) 

ejecutarMision:: Mision -> [Ninja]-> Bool
ejecutarMision unaMision grupoDeNinjas = esCopadaOFactible grupoDeNinjas . todosUsanJutus unaMision $ grupoDeNinjas

esCopadaOFactible:: [Ninja] -> Mision ->  Bool
esCopadaOFactible  grupoDeNinjas unaMision  = esCopada unaMision || esFactible unaMision grupoDeNinjas

usarJutsus:: Mision ->  Ninja -> Mision
usarJutsus unaMision  = foldl (flip ($)) unaMision . jutsus

todosUsanJutus:: Mision -> [Ninja] -> Mision
todosUsanJutus = foldl usarJutsus 

-- C

granGuerraNinja:: Mision
granGuerraNinja = Mision {cantidadRequerida = 100000, rangoMision = 100, enemigos = enemigosInfinitos, recompensa = abanicoDeMadara}

abanicoDeMadara:: Herramienta
abanicoDeMadara = Herramienta "abanico de Madara Uchiha" 1 

enemigosInfinitos:: [Ninja]
enemigosInfinitos = map (\n -> Ninja ("Zetsu " ++ show n) [] [] 600) [1..]

{-
a. No devuelve nada ya que tiene que saber la longitud de los enemigos y no lo tendrá nunca porque es infinita.
b. Devuelve False porque no evalua los enemigos ni los ninjas, solo la recompensa y la recompensa de la misión no es 3 bombas de humo, 5 shurikens o 14 kunais.
c. Devuelve satisfactoriamente la misión despues de aplicar el jutsu y va mostrando los ninjas enemigos a medida que los va teniendo.

-}
