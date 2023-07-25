import Text.Show.Functions

---------- Datas y Tipos ----------

data Auto = Auto {
  nombre :: Nombre,
  nafta :: Int,
  velocidad :: Int,
  enamorade :: String,
  truco :: Truco
} deriving Show

type Nombre = String
type Truco = Carrera -> Auto -> Auto
type Distancia = Int

data Carrera = Carrera {
  vueltas :: Int,
  longitudPista :: Int,
  participantes :: Participantes,
  publico :: Publico
} deriving Show

type Participantes = [Auto]
type Publico = [String]


carrerarda :: Carrera
carrerarda = Carrera 5 100 [rodra,gushtav] []

rochaMcQueen :: Auto
rochaMcQueen = Auto "rochaMcQueen" 282 0 "Ronco" deReversaRocha

biankerr :: Auto
biankerr = Auto "biankerr" 378 0 "Tincho" deReversaRocha

gushtav :: Auto
gushtav = Auto "gushtav" 100 0 "Peti" deReversaRocha

rodra :: Auto
rodra = Auto "rodra" 153 0 "Tais" deReversaRocha

--- Funciones auxiliares


mapNafta :: (Int -> Int) -> Auto -> Auto
mapNafta f unAuto = unAuto { nafta = f . nafta $ unAuto }

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad f unAuto = unAuto { velocidad = f . velocidad $ unAuto }

deReversaRocha :: Truco
deReversaRocha carrera = mapNafta . (+) . (* 5). longitudPista $ carrera

-- Vuelta de carrera

longitudNombre :: Auto -> Int
longitudNombre = length . nombre

restarCombustibleVuelta :: Carrera -> Auto -> Auto
restarCombustibleVuelta carrera unAuto = mapNafta (subtract . (* (longitudNombre unAuto)) . longitudPista $ carrera) unAuto

incrementoVelocidadVuelta :: Auto -> Int
incrementoVelocidadVuelta unAuto
        | longitudNombre unAuto <= 5 = 15
        | longitudNombre unAuto <= 8 = 20
        | otherwise = 30

{-velocidadVuelta :: Auto -> Auto
velocidadVuelta unAuto = modificarVelocidad (incrementoVelocidadVuelta unAuto) unAuto

compararVelocidades :: Carrera -> Auto -> Auto -> Auto
compararVelocidades carrera auto1 auto2
        | velocidad auto1 > velocidad auto2 = truco carrera 
        | otherwise = truco carrera auto1

--velocidadMasBajaVuelta :: 
-}