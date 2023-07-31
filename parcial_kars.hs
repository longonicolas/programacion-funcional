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
  participantes :: [Auto],
  publico :: Publico
} deriving Show

type Publico = [String]


carrerarda :: Carrera
carrerarda = Carrera 6 5 [rodra,gushtav] []

rochaMcQueen :: Auto
rochaMcQueen = Auto "rochaMcQueen" 282 10 "Ronco" deReversaRocha

biankerr :: Auto
biankerr = Auto "biankerr" 378 15 "Tincho" deReversaRocha

gushtav :: Auto
gushtav = Auto "gushtav" 100 7 "Peti" deReversaRocha

rodra :: Auto
rodra = Auto "rodra" 153 8 "Tais" deReversaRocha

--- Funciones auxiliares


mapNafta :: (Int -> Int) -> Auto -> Auto
mapNafta f unAuto = unAuto{ nafta = f . nafta $ unAuto }

mapVelocidad :: (Int -> Int) -> Auto -> Auto
mapVelocidad f unAuto = unAuto{ velocidad = f . velocidad $ unAuto }

mapCarrera :: ([Auto] -> [Auto]) -> Carrera -> Carrera
mapCarrera funcion carrera = carrera{ participantes = funcion . participantes $ carrera}

--------------------------------------------------------------------------
deReversaRocha :: Truco
deReversaRocha carrera = mapNafta . (+) . (*5) . longitudPista $ carrera

-- Vuelta de carrera

longitudNombre :: Auto -> Int
longitudNombre = length . nombre

restarCombustibleVueltaUnAuto :: Carrera -> Auto -> Auto
restarCombustibleVueltaUnAuto carrera unAuto = mapNafta (subtract . (* (longitudNombre unAuto)) . longitudPista $ carrera) unAuto

restarCombustibleVuelta :: Carrera -> Carrera
restarCombustibleVuelta unaCarrera = mapCarrera (map $ restarCombustibleVueltaUnAuto unaCarrera) unaCarrera

-----------------------------------------------------------------------

incrementoVelocidadVuelta :: Auto -> Int
incrementoVelocidadVuelta unAuto
        | longitudNombre unAuto <= 5 = 15
        | longitudNombre unAuto <= 8 = 20
        | otherwise = 30

aumentarVelocidadVueltaUnAuto :: Auto -> Auto
aumentarVelocidadVueltaUnAuto unAuto = mapVelocidad ( (+) . incrementoVelocidadVuelta $ unAuto) unAuto

aumentarVelocidadVuelta :: Carrera -> Carrera
aumentarVelocidadVuelta  = mapCarrera . map $ aumentarVelocidadVueltaUnAuto

-------------------------------------------------------------------------

masLentoEntreDosAutos :: Auto -> Auto -> Auto
masLentoEntreDosAutos auto1 auto2
    | velocidad auto1 > velocidad auto2 = auto2
    | otherwise = auto1

autoMasLentoDeLaCarrera :: Carrera -> Auto
autoMasLentoDeLaCarrera carrera = foldl1 masLentoEntreDosAutos . participantes $ carrera

esElAutoMasLento :: Carrera -> Auto -> Bool
esElAutoMasLento carrera unAuto = nombre unAuto == (nombre . autoMasLentoDeLaCarrera $ carrera)

aplicarTruco :: Carrera -> Auto -> Auto
aplicarTruco carrera unAuto = (truco unAuto) carrera unAuto

aplicarTrucoAutoMasLento :: Carrera -> Auto
aplicarTrucoAutoMasLento carrera = aplicarTruco carrera . autoMasLentoDeLaCarrera $ carrera

participantesRapidos :: Carrera -> [Auto] -> [Auto]
participantesRapidos carrera autos = (filter (not.esElAutoMasLento carrera)) $ autos

agregarParticipantesRapidos :: Carrera -> Carrera
agregarParticipantesRapidos carrera = mapCarrera (participantesRapidos carrera) carrera

agregarAutoLento :: Carrera -> Carrera
agregarAutoLento carrera = mapCarrera ((:) . aplicarTrucoAutoMasLento $ carrera) carrera

agregarAutos :: Carrera -> Carrera
agregarAutos carrera =  mapCarrera ((aplicarTrucoAutoMasLento carrera) : (participantes $ agregarParticipantesRapidos carrera)) carrera
--------------------------------------------------------------------------

--darVuelta :: Carrera -> Carrera
--darVuelta unaCarrera = restarCombustibleVuelta . aumentarVelocidadVuelta $ unaCarrera