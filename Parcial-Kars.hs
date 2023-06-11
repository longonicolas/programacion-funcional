import Text.Show.Functions()

data Carrera = Carrera{

    nroVueltas :: Int,
    longitudPista :: Int,
    participantes :: [Auto],
    publico :: [String]
}deriving Show

type Truco = Auto -> Auto

data Auto = Auto{

    nombre :: String,
    nafta :: Int,
    velocidad :: Int,
    nombreEnamorado :: String,
    trucardo :: Truco
}deriving Show

rochaMcQueen = Auto "RochaMcQueen" 282 0 "Ronco" (deReversaRocha 100)
biankerr = Auto "Biankerr" 378 0 "Tincho" (impresionar ["Tincho"])
gushtav = Auto "Gushtav" 230 0 "Peti" nitro
rodra = Auto "Rodra" 153 0 "Tais" (comboLoco 100)

carrerarda = Carrera 4 2000 [rochaMcQueen,biankerr] ["Ronco","Tincho","Juan"]

deReversaRocha :: Int -> Truco
deReversaRocha largoPista unAuto = modificarNafta (5 * largoPista) unAuto

modificarNafta :: Int -> Auto -> Auto
modificarNafta cantidad unAuto = unAuto{nafta = nafta unAuto + cantidad}

impresionar :: [String] -> Truco
impresionar publico unAuto
        | estaElEnamorado publico unAuto = modificarVelocidad (velocidad unAuto) unAuto
        | otherwise = unAuto
    
estaElEnamorado :: [String] -> Auto -> Bool
estaElEnamorado publico unAuto = any (== nombreEnamorado unAuto) publico

modificarVelocidad :: Int -> Auto -> Auto
modificarVelocidad cantidad unAuto = unAuto{velocidad = velocidad unAuto + cantidad}

nitro :: Truco
nitro unAuto = modificarVelocidad 15 unAuto

comboLoco :: Int -> Truco
comboLoco largoPista unAuto = deReversaRocha largoPista . nitro $ unAuto

-- CARRERAS
type Vuelta = Auto -> Auto

metrosAKm :: Int -> Int
metrosAKm metros = div metros 1000

restarCombustible :: Int -> Auto -> Auto
restarCombustible largoPista unAuto = modificarNafta ((metrosAKm largoPista) * ((*(-1)) . length . nombre $ unAuto)) unAuto

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad unAuto
        | length (nombre unAuto) <= 5 = modificarVelocidad 15 unAuto
        | length (nombre unAuto) <= 8 = modificarVelocidad 20 unAuto
        | otherwise = modificarVelocidad 30 unAuto
 
 -- velocidad mas baja

autoLento :: Auto -> Auto -> Auto
autoLento unAuto otroAuto
        | velocidad unAuto < velocidad otroAuto = unAuto
        | otherwise = otroAuto 

--estaElAuto :: Auto -> [Auto] -> Bool
--estaElAuto unAuto autos = any (== nombre unAuto) (map (obtenerNombre) autos)

autoMasLento :: [Auto] -> Auto
autoMasLento autos = aplicarTruco (trucardo (foldl1 autoLento autos)) (foldl1 autoLento autos)

agregarAutoACarrera :: Carrera -> Carrera
agregarAutoACarrera carrera = carrera{participantes = autoMasLento (participantes carrera) : participantes carrera}

-- termina velocidad mas baja

aplicarTruco :: Truco -> Auto -> Auto
aplicarTruco truco unAuto = truco unAuto

darVuelta :: Carrera -> Carrera
darVuelta carrera = agregarAutoACarrera . modificarParticipantes $ carrera

modificarParticipantes :: Carrera -> Carrera
modificarParticipantes unaCarrera = unaCarrera{participantes = map (efectoVuelta (longitudPista unaCarrera)) (participantes unaCarrera)}

efectoVuelta :: Int -> Auto -> Auto
efectoVuelta largoPista unAuto = restarCombustible largoPista . incrementarVelocidad $ unAuto

correrCarrera :: Carrera -> Carrera
correrCarrera carrera = replicate (nroVueltas carrera) darVuelta